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

  return std::make_pair(std::move(calculateDigest(GO)), true);
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

// Return true if GO is a function is defined in this module.
static bool isDefinition(const GlobalObject &GO) {
  return !GO.isDeclaration() && !GO.hasAvailableExternallyLinkage();
}

static const DependenciesType &
updateInitialDigestAndGetDependencies(const GlobalObject *GO, MD5 &GOHash,
                                      GOInfoMap &GOIMap, GONumber &GONum) {
  GOInfoMap::const_iterator Pos;
  if (const auto GV = dyn_cast<GlobalVariable>(GO)) {
    ++GONum.VarNum;
    Pos = calculateInitialDigestAndDependencies(GV, GOIMap);
  } else if (const auto Fn = dyn_cast<Function>(GO)) {
    ++GONum.FuncNum;
    Pos = calculateInitialDigestAndDependencies(Fn, GOIMap);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
  GOHash.update(Pos->second.InitialDigest.Bytes);
  return Pos->second.Dependencies;
}

const DependenciesType &
updateDigestGONumAndGetDependencies(const GlobalObject *GO, MD5 &GOHash,
                                    GOInfoMap &GOIMap, GONumber &GONum) {
  auto It = GOIMap.find(GO);
  if (It != GOIMap.end()) {
    const GOInfo &GOInformation = It->second;
    GOHash.update(GOInformation.InitialDigest.Bytes);
    return GOInformation.Dependencies;
  }

  return updateInitialDigestAndGetDependencies(GO, GOHash, GOIMap, GONum);
}

const DependenciesType &updateDigestAndGetDependencies(const GlobalObject *GO,
                                                       MD5 &GOHash,
                                                       GOInfoMap &GOIMap) {
  if (auto GOMD = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *TN = dyn_cast<TicketNode>(GOMD)) {
      DigestType D = TN->getDigest();
      GOHash.update(D.Bytes);
      return GOIMap.try_emplace(GO, std::move(D), std::move(DependenciesType()))
          .first->second.Dependencies;
    }
    report_fatal_error("Failed to get TicketNode metadata!");
  }
  GONumber GONum;
  return updateInitialDigestAndGetDependencies(GO, GOHash, GOIMap, GONum);
}

// Update the GO's hash value by adding the hash of its dependents.
template <typename Function>
static void
updateDigestUseCallDependencies(const GlobalObject *GO, MD5 &GOHash,
                                GOStateMap &Visited, GOInfoMap &GOIMap,
                                Function UpdateDigestAndGetDependencies) {
  if (!isDefinition(*GO))
    return;

  bool Inserted;
  typename GOStateMap::const_iterator StateIt;
  std::tie(StateIt, Inserted) = Visited.try_emplace(GO, Visited.size());
  if (!Inserted) {
    // If GO is visited, use the letter 'R' as the marker and use its state as
    // the value.
    GOHash.update('R');
    GOHash.update(StateIt->second);
    return;
  }

  GOHash.update('T');
  auto Dependencies = UpdateDigestAndGetDependencies(GO, GOHash, GOIMap);

  // Recursively for all the dependent global objects.
  for (const GlobalObject *D : Dependencies)
    updateDigestUseCallDependencies(D, GOHash, Visited, GOIMap,
                                    UpdateDigestAndGetDependencies);
}

std::tuple<bool, unsigned, unsigned> generateTicketMDs(Module &M) {
  bool Changed = false;
  GONumber GONum;
  // Calculate the final GO hash by adding the initial hashes of its dependents
  // and create the ticket metadata for GOs.
  GOStateMap Visited;
  GOInfoMap GOIMap;
  for (auto &GO : M.global_objects()) {
    if (!isDefinition(GO))
      continue;
    Visited.clear();
    MD5 Hash = MD5();
    auto Helper = [&GONum](const GlobalObject *GO, MD5 &GOHash,
                           GOInfoMap &GOIMap) {
      return updateDigestGONumAndGetDependencies(GO, GOHash, GOIMap, GONum);
    };
    updateDigestUseCallDependencies(&GO, Hash, Visited, GOIMap, Helper);
    MD5::MD5Result Digest;
    Hash.final(Digest);
    set(&GO, Digest);
    Changed = true;
  }

  return std::make_tuple(Changed, GONum.VarNum, GONum.FuncNum);
}

DigestType calculateDigest(const GlobalObject *GO) {
  GOStateMap Visited;
  MD5 Hash = MD5();
  GOInfoMap GOIMap;

  updateDigestUseCallDependencies(GO, Hash, Visited, GOIMap,
                                  updateDigestAndGetDependencies);
  MD5::MD5Result Digest;
  Hash.final(Digest);
  return std::move(Digest);
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
