//===----  RepoPruning.cpp - Program repository pruning  ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoGlobals.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/IR/RepoTicket.h"
#include "llvm/Pass.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/GlobalStatus.h"
#include <iostream>
#include <set>
using namespace llvm;

#define DEBUG_TYPE "prepo-pruning"

STATISTIC(NumAliases, "Number of global aliases removed");
STATISTIC(NumFunctions, "Number of functions removed");
STATISTIC(NumVariables, "Number of variables removed");

namespace {

/// RepoPruning removes the redundant global objects.
class RepoPruning : public ModulePass {
public:
  static char ID;
  RepoPruning() : ModulePass(ID) {
    initializeRepoPruningPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override { return "RepoPruningPass"; }

  bool runOnModule(Module &M) override;

private:
  bool isObjFormatRepo(Module &M) const {
    return Triple(M.getTargetTriple()).isOSBinFormatRepo();
  }
};

} // end anonymous namespace

char RepoPruning::ID = 0;
INITIALIZE_PASS(RepoPruning, "prepo-pruning",
                "Program Repository Global Object Pruning", false, false)

ModulePass *llvm::createRepoPruningPass() { return new RepoPruning(); }

using GlobalObjectMap =
    std::map<const GlobalObject *, const ticketmd::DigestType>;

GlobalValue::LinkageTypes toGVLinkage(pstore::repo::linkage_type L) {
  switch (L) {
  case pstore::repo::linkage_type::external:
    return GlobalValue::ExternalLinkage;
  case pstore::repo::linkage_type::linkonce:
    return GlobalValue::LinkOnceAnyLinkage;
  case pstore::repo::linkage_type::internal:
    return GlobalValue::InternalLinkage;
  case pstore::repo::linkage_type::common:
    return GlobalValue::CommonLinkage;
  case pstore::repo::linkage_type::append:
    return GlobalValue::AppendingLinkage;
  default:
    report_fatal_error("Unsupported linkage type");
  }
}

StringRef toStringRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}

ticketmd::DigestType toDigestType(pstore::index::digest D) {
  ticketmd::DigestType Digest;
  support::endian::write64le(&Digest, D.low());
  support::endian::write64le(&(Digest.Bytes[8]), D.high());
  return Digest;
}

bool RepoPruning::runOnModule(Module &M) {
  if (skipModule(M) || !isObjFormatRepo(M))
    return false;

  MDBuilder MDB(M.getContext());

  pstore::database &Repository = getRepoDatabase();

  std::shared_ptr<pstore::index::digest_index const> const Digests =
      pstore::index::get_digest_index(Repository, false);
  if (!Digests) {
    return false;
  }

  // Erase the unchanged global objects.
  auto EraseUnchangedGlobalObect = [&Digests, &Repository,
                                    &M](GlobalObject &GO,
                                        llvm::Statistic &NumGO) -> bool {
    if (GO.isDeclaration() || GO.hasAvailableExternallyLinkage())
      return false;
    auto const Result = ticketmd::get(&GO);
    assert(!Result.second && "The repo_ticket metadata should be created by "
                             "the RepoTicketGeneration pass!");

    auto const Key =
        pstore::index::digest{Result.first.high(), Result.first.low()};
    auto it = Digests->find(Key);
    if (it == Digests->end())
      return false;

    ++NumGO;
    GO.setComdat(nullptr);
    // Remove all metadata except fragment.
    TicketNode *MD =
        dyn_cast<TicketNode>(GO.getMetadata(LLVMContext::MD_repo_ticket));
    MD->setPruned(true);
    GO.clearMetadata();
    GO.setMetadata(LLVMContext::MD_repo_ticket, MD);
    GO.setLinkage(GlobalValue::ExternalLinkage);
    // Create  the dependent fragments if existing.
    auto Fragment = pstore::repo::fragment::load(Repository, it->second);
    if (auto Dependents = Fragment->dependents()) {
      for (pstore::typed_address<pstore::repo::ticket_member> Dependent :
           *Dependents) {
        auto TM = pstore::repo::ticket_member::load(Repository, Dependent);
        StringRef MDName =
            toStringRef(pstore::get_sstring_view(Repository, TM->name).second);
        auto DMD =
            TicketNode::get(M.getContext(), MDName, toDigestType(TM->digest),
                            toGVLinkage(TM->linkage), true);
        NamedMDNode *const NMD = M.getOrInsertNamedMetadata("repo.tickets");
        assert(NMD && "NamedMDNode cannot be NULL!");
        NMD->addOperand(DMD);
      }
    }
    return true;
  };

  bool Changed = false;
  for (GlobalVariable &GV : M.globals()) {
    if (EraseUnchangedGlobalObect(GV, NumVariables)) {
      // Removes the Global variable initializer.
      Constant *Init = GV.getInitializer();
      if (isSafeToDestroyConstant(Init))
        Init->destroyConstant();
      GV.setInitializer(nullptr);
      Changed = true;
    }
  }

  for (Function &Func : M) {
    if (EraseUnchangedGlobalObect(Func, NumFunctions)) {
      auto MD = Func.getMetadata(LLVMContext::MD_repo_ticket);
      Func.deleteBody();
      Func.setMetadata(LLVMContext::MD_repo_ticket, MD);
      Changed = true;
    }
  }

  DEBUG(dbgs() << "size of module: " << M.size() << '\n');
  DEBUG(dbgs() << "size of removed functions: " << NumFunctions << '\n');
  DEBUG(dbgs() << "size of removed variables: " << NumVariables << '\n');
  DEBUG(dbgs() << "size of removed aliases: " << NumAliases << '\n');

  return Changed;
}
