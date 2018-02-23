//===---- RepoTicket.cpp -  Implement digest data structure. ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the digest data structure.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoTicket.h"
#include "LLVMContextImpl.h"
#include "MetadataImpl.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>

using namespace llvm;

namespace llvm {

void Digest::set(GlobalObject *GO, Digest::DigestType const &D) {
  auto M = GO->getParent();
  MDBuilder MDB(M->getContext());
  auto MD = MDB.createTicketNode(GO->getName(), D, GO->getLinkage());
  assert(MD && "TicketNode cannot be NULL!");
  GO->setMetadata(LLVMContext::MD_repo_ticket, MD);
  NamedMDNode *NMD = M->getOrInsertNamedMetadata("repo.tickets");
  assert(NMD && "NamedMDNode cannot be NULL!");
  NMD->addOperand(MD);
}

auto Digest::get(const GlobalObject *GO)
    -> std::pair<Digest::DigestType, bool> {

  if (const auto *const T = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *const MD = dyn_cast<TicketNode>(T)) {
      return std::make_pair(MD->getDigest(), false);
    }
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Failed to get TicketNode metadata for global object '" +
                       GO->getName() + "'.");
  }

  if (const auto *const GVar = dyn_cast<GlobalVariable>(GO)) {
    return std::make_pair(calculateDigest<GlobalVariable>(GVar), true);
  }

  if (const auto *const GF = dyn_cast<Function>(GO)) {
    return std::make_pair(calculateDigest<Function>(GF), true);
  }

  llvm_unreachable("Unknown global object type!");
}

const Constant *Digest::getAliasee(const GlobalAlias *GA) {
  auto Aliasee = GA->getAliasee();
  assert(Aliasee && "Aliasee cannot be NULL!");
  auto Target = Aliasee->stripPointerCasts();
  assert(Target && "Target cannot be NULL!");
  // After stripping pointer casts, the target type should be only
  // GlobalValue type.
  assert(isa<GlobalValue>(Target) && "Aliasee should be only GlobalValue");
  return Target;
}

#ifndef NDEBUG
static bool isCanonical(const MDString *S) {
  return !S || !S->getString().empty();
}
#endif

TicketNode *TicketNode::getImpl(LLVMContext &Context, MDString *Name,
                                ConstantAsMetadata *Digest,
                                GlobalValue::LinkageTypes Linkage, bool Pruned,
                                StorageType Storage, bool ShouldCreate) {
  if (Storage == Uniqued) {
    if (auto *N =
            getUniqued(Context.pImpl->TicketNodes,
                       TicketNodeInfo::KeyTy(Linkage, Pruned, Name, Digest)))
      return N;
    if (!ShouldCreate)
      return nullptr;
  } else {
    assert(ShouldCreate && "Expected non-uniqued nodes to always be created");
  }

  assert(isCanonical(Name) && "Expected canonical MDString");
  Metadata *Ops[] = {Name, Digest};
  return storeImpl(new (array_lengthof(Ops))
                       TicketNode(Context, Storage, Linkage, Pruned, Ops),
                   Storage, Context.pImpl->TicketNodes);
}

TicketNode *TicketNode::getImpl(LLVMContext &Context, StringRef Name,
                                Digest::DigestType const &Digest,
                                GlobalValue::LinkageTypes Linkage, bool Pruned,
                                StorageType Storage, bool ShouldCreate) {
  MDString *MDName = nullptr;
  if (!Name.empty())
    MDName = MDString::get(Context, Name);
  MDBuilder MDB(Context);
  const auto Size = Digest::DigestSize;
  llvm::Constant *Field[Size];
  Type *Int8Ty = Type::getInt8Ty(Context);
  for (unsigned Idx = 0; Idx < Size; ++Idx) {
    Field[Idx] = llvm::ConstantInt::get(Int8Ty, Digest[Idx], false);
  }
  // Array implementation that the hash is outputed as char/string.
  ConstantAsMetadata *MDDigest = ConstantAsMetadata::get(
      ConstantArray::get(llvm::ArrayType::get(Int8Ty, Size), Field));
  return getImpl(Context, MDName, MDDigest, Linkage, Pruned, Storage,
                 ShouldCreate);
}

Digest::DigestType TicketNode::getDigest() const {
  ConstantAsMetadata const *C = getDigestAsMDConstant();
  auto const ArrayType = C->getType();
  auto const Elems = ArrayType->getArrayNumElements();
  Digest::DigestType D;

  assert(Elems == D.Bytes.max_size() &&
         "Global object has invalid digest array size.");
  for (unsigned I = 0, E = Elems; I != E; ++I) {
    ConstantInt const *CI =
        dyn_cast<ConstantInt>(C->getValue()->getAggregateElement(I));
    assert(CI);
    D[I] = CI->getValue().getZExtValue();
  }
  return D;
}

} // end namespace llvm
