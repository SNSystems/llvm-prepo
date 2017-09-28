//===------ Digest.cpp -  Implement digest data structure. ------*- C++ -*-===//
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

#include "llvm/IR/Digest.h"
#include "LLVMContextImpl.h"
#include "MetadataImpl.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>

using namespace llvm;

namespace llvm {

class MDNode;

void Digest::set(Module const &M, GlobalObject *GO,
                 Digest::DigestType const &D) {
  MDBuilder MDB(M.getContext());
  GO->setMetadata(LLVMContext::MD_fragment,
                  MDB.createTicketNode(GO->getName(), D, GO->getLinkage(),
                                       GO->getComdat() != nullptr));
}

Digest::DigestType Digest::get(const GlobalObject *GO) {
  // Use the digest in the MCSectionRepo.
  TicketNode *MD =
      dyn_cast<TicketNode>(GO->getMetadata(LLVMContext::MD_fragment));
  if (!MD) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Failed to get TicketNode metadata for global object '" +
                       GO->getName() + "'.");
  }
  return MD->getDigest();
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

void Digest::createDigestFile(Module const &M, GlobalValueMap const &GVMap,
                              StringRef FileExt) {
  const std::string &MId = M.getModuleIdentifier();
  std::string hashFName(MId.begin(), find(MId, '.') + 1);
  hashFName += FileExt;
  std::error_code EC;
  raw_fd_ostream OS(hashFName, EC, sys::fs::F_Text);
  if (EC) {
    errs() << "Couldn't open " << M.getModuleIdentifier()
           << " for generating hash.\nError:" << EC.message() << "\n";
    exit(1);
  }

  for (auto const &GO : GVMap) {
    if (dyn_cast<GlobalVariable>(GO.first)) {
      OS << "Global variable name: ";
    } else if (dyn_cast<Function>(GO.first)) {
      OS << "Global function name: ";
    } else if (dyn_cast<GlobalAlias>(GO.first)) {
      auto Target = getAliasee(dyn_cast<GlobalAlias>(GO.first));
      OS << "Global Alias name ";
      OS << " (Alias to: ";
      if (auto GVA = dyn_cast<GlobalVariable>(Target)) {
        OS << "a global variable: " << Target->getName() << ") : ";
      } else if (auto GVF = dyn_cast<Function>(Target)) {
        OS << "a function: " << GVF->getName() << ") : ";
      } else {
        assert(false && "Unknown GlobalObject type!");
      }
    }
    OS << '\t' << GO.first->getName() << ": digest:" << GO.second.digest()
       << '\n';
  }
}

#ifndef NDEBUG
static bool isCanonical(const MDString *S) {
  return !S || !S->getString().empty();
}
#endif

TicketNode *TicketNode::getImpl(LLVMContext &Context, MDString *Name,
                                ConstantAsMetadata *Digest,
                                GlobalValue::LinkageTypes Linkage,
                                bool IsComdat, StorageType Storage,
                                bool ShouldCreate) {
  if (Storage == Uniqued) {
    if (auto *N =
            getUniqued(Context.pImpl->TicketNodes,
                       TicketNodeInfo::KeyTy(Linkage, IsComdat, Name, Digest)))
      return N;
    if (!ShouldCreate)
      return nullptr;
  } else {
    assert(ShouldCreate && "Expected non-uniqued nodes to always be created");
  }

  assert(isCanonical(Name) && "Expected canonical MDString");
  Metadata *Ops[] = {Name, Digest};
  return storeImpl(new (array_lengthof(Ops))
                       TicketNode(Context, Storage, Linkage, IsComdat, Ops),
                   Storage, Context.pImpl->TicketNodes);
}

TicketNode *TicketNode::getImpl(LLVMContext &Context, StringRef Name,
                                Digest::DigestType const &Digest,
                                GlobalValue::LinkageTypes Linkage,
                                bool IsComdat, StorageType Storage,
                                bool ShouldCreate) {
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
  return getImpl(Context, MDName, MDDigest, Linkage, IsComdat, Storage,
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
