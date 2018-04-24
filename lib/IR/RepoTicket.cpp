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

namespace ticketmd {

void set(GlobalObject *GO, ticketmd::DigestType const &D) {
  auto M = GO->getParent();
  MDBuilder MDB(M->getContext());
  auto MD = MDB.createTicketNode(GO->getName(), D, GO->getLinkage());
  assert(MD && "TicketNode cannot be NULL!");
  GO->setMetadata(LLVMContext::MD_repo_ticket, MD);
  NamedMDNode *NMD = M->getOrInsertNamedMetadata("repo.tickets");
  assert(NMD && "NamedMDNode cannot be NULL!");
  NMD->addOperand(MD);
}

auto get(const GlobalObject *GO) -> std::pair<ticketmd::DigestType, bool> {

  if (const auto *const T = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *const MD = dyn_cast<TicketNode>(T)) {
      return std::make_pair(MD->getDigest(), false);
    }
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Failed to get TicketNode metadata for global object '" +
                       GO->getName() + "'.");
  }

  if (const auto *const GVar = dyn_cast<GlobalVariable>(GO)) {
    return std::make_pair(
        // TODO: Should update the hash using the dependent list?
        std::move(calculateDigestAndDependencies<GlobalVariable>(GVar).first),
        true);
  }

  if (const auto *const GF = dyn_cast<Function>(GO)) {
    // TODO: Should update the hash using the dependent list?
    return std::make_pair(
        std::move(calculateDigestAndDependencies<Function>(GF).first), true);
  }

  llvm_unreachable("Unknown global object type!");
}

const Constant *getAliasee(const GlobalAlias *GA) {
  auto Aliasee = GA->getAliasee();
  assert(Aliasee && "Aliasee cannot be NULL!");
  auto Target = Aliasee->stripPointerCasts();
  assert(Target && "Target cannot be NULL!");
  // After stripping pointer casts, the target type should be only
  // GlobalValue type.
  assert(isa<GlobalValue>(Target) && "Aliasee should be only GlobalValue");
  return Target;
}

static bool isDefinition(const GlobalObject &GO) {
  return !GO.isDeclaration() && !GO.hasAvailableExternallyLinkage();
}

// Note: this function is not static function since it is used for unit test as
// well.
ModuleTuple calculateInitialDigestAndDependencies(Module &M) {
  GOInfoMap Result;
  DigestAndDependencies DD;
  unsigned GVNum = 0, FnNum = 0;
  for (auto &GO : M.global_objects()) {
    if (!isDefinition(GO))
      continue;
    if (const GlobalVariable *GV = dyn_cast<GlobalVariable>(&GO)) {
      DD = calculateDigestAndDependencies<GlobalVariable>(GV);
      Result.try_emplace(&GO, std::move(DD.first), std::move(DD.second));
      ++GVNum;
    } else if (const Function *Fn = dyn_cast<Function>(&GO)) {
      DD = calculateDigestAndDependencies<Function>(Fn);
      Result.try_emplace(&GO, std::move(DD.first), std::move(DD.second));
      ++FnNum;
    } else {
      llvm_unreachable("Unknown global object type!");
    }
  }
  return std::make_tuple(std::move(Result), GVNum, FnNum);
}

// Update the GO's hash value by adding the hash of its dependents.
static unsigned updateDigestUseCallDependencies(const GlobalObject *GO,
                                                GOInfoMap &GOI, MD5 &GOHash,
                                                unsigned NumVisited = 0) {
  auto It = GOI.find(GO);
  assert(It != GOI.end());
  GOInfo &GOInformation = It->second;
  if (GOInformation.Visited) {
    // If GO is visited, use the letter 'R' as the marker and use its Index as
    // the value.
    GOHash.update('R');
    GOHash.update(GOInformation.Index);
    return NumVisited;
  }

  GOHash.update('T');
  GOHash.update(GOInformation.InitialDigest.Bytes);
  // Set the 'Visited' to true and assigned the identical number to 'Index'.
  GOInformation.Visited = true;
  GOInformation.Index = ++NumVisited;

  // Recursively for all the dependent global objects.
  for (const GlobalObject *D : GOInformation.Dependencies)
    NumVisited = updateDigestUseCallDependencies(D, GOI, GOHash, NumVisited);

  return NumVisited;
}

std::tuple<bool, unsigned, unsigned> generateTicketMDs(Module &M) {
  bool Changed = false;

  // Step 1: calculate the initial GO information.
  auto GOTuple = calculateInitialDigestAndDependencies(M);
  GOInfoMap &GOIMap = std::get<0>(GOTuple);

  // Step 2: calculate the final GO hash by adding the hashes of its dependents
  // and create the ticket metadata for GOs.
  for (auto &GO : M.global_objects()) {
    if (!isDefinition(GO))
      continue;
    MD5 Hash = MD5();
    // TODO: Move Visited and Index to anther DenseMap.
    // Reset the visited flag in the global objects information.
    for (auto &Elem : GOIMap) {
      Elem.second.Visited = false;
    }
    updateDigestUseCallDependencies(&GO, GOIMap, Hash);
    MD5::MD5Result Digest;
    Hash.final(Digest);
    set(&GO, Digest);
    Changed = true;
  }

  return std::make_tuple(Changed, std::get<1>(GOTuple), std::get<2>(GOTuple));
}

} // namespace ticketmd
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
                                ticketmd::DigestType const &Digest,
                                GlobalValue::LinkageTypes Linkage, bool Pruned,
                                StorageType Storage, bool ShouldCreate) {
  MDString *MDName = nullptr;
  if (!Name.empty())
    MDName = MDString::get(Context, Name);
  MDBuilder MDB(Context);
  const auto Size = ticketmd::DigestSize;
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

ticketmd::DigestType TicketNode::getDigest() const {
  ConstantAsMetadata const *C = getDigestAsMDConstant();
  auto const ArrayType = C->getType();
  auto const Elems = ArrayType->getArrayNumElements();
  ticketmd::DigestType D;

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
