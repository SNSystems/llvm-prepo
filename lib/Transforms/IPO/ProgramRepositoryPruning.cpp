//===----  ProgramRepositoryPruning.cpp - Program repository pruning  ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoTicket.h"
#include "llvm/Pass.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/GlobalStatus.h"
#include "llvm/Transforms/Utils/RepoHashCalculator.h"
#include <set>
#include "pstore/database.hpp"
#include "pstore/hamt_map.hpp"
#include <iostream>
using namespace llvm;

#define DEBUG_TYPE "prepo-pruning"

STATISTIC(NumAliases, "Number of global aliases removed");
STATISTIC(NumFunctions, "Number of functions removed");
STATISTIC(NumVariables, "Number of variables removed");

namespace {

/// ProgramRepositoryPruning removes the redundant global objects.
class ProgramRepositoryPruning : public ModulePass {
public:
  static char ID;
  ProgramRepositoryPruning() : ModulePass(ID) {
    initializeProgramRepositoryPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override { return "PrepoPruningPass"; }

  bool runOnModule(Module &M) override;

private:
  bool isObjFormatRepo(Module &M) const {
    return Triple(M.getTargetTriple()).isOSBinFormatRepo();
  }
};

} // end anonymous namespace

char ProgramRepositoryPruning::ID = 0;
INITIALIZE_PASS(ProgramRepositoryPruning, "prepo-pruning",
                "Program Repository Global Object Pruning", false, false)

ModulePass *llvm::createProgramRepositoryPruningPass() {
  return new ProgramRepositoryPruning();
}

using GlobalObjectMap =
    std::map<const GlobalObject *, const Digest::DigestType>;
using GlobalValueMap = Digest::GlobalValueMap;

bool ProgramRepositoryPruning::runOnModule(Module &M) {
  if (skipModule(M) || !isObjFormatRepo(M))
    return false;

  bool Changed = false;
  MDBuilder MDB(M.getContext());

  static pstore::database Repository ("./clang.db", true/*writable*/); // FIXME: share an instance with the repo-writer back-end!

  MDNode *MD = nullptr;
  pstore::index::digest_index * Digests = Repository.get_digest_index (false);
  if (Digests == nullptr) {
    return false;
  }

  // Erase the unchanged global objects.
  auto EraseUnchangedGlobalObect = [&](GlobalObject &GO,
                                       llvm::Statistic &NumGO) -> bool {
    if (GO.isDeclaration() || GO.hasAvailableExternallyLinkage())
      return false;
    auto Result = Digest::get(&GO);

    auto const Key = pstore::index::uint128 {Result.high (), Result.low ()};
    if (Digests->find (Key) != Digests->end ()) {
      Changed = true;
      ++NumGO;
      GO.setComdat(nullptr);
      // Remove all metadata except fragment.
      MD = GO.getMetadata(LLVMContext::MD_fragment);
      GO.clearMetadata();
      GO.setMetadata(LLVMContext::MD_fragment, MD);
      GO.setLinkage(GlobalValue::ExternalLinkage);
      return true;
    }
    return false;
  };

  for (GlobalVariable &GV : M.globals()) {
    if (EraseUnchangedGlobalObect(GV, NumVariables)) {
      // Removes the Global variable initializer.
      Constant *Init = GV.getInitializer();
      if (isSafeToDestroyConstant(Init))
        Init->destroyConstant();
      GV.setInitializer(nullptr);
    }
  }

  for (Function &Func : M) {
    if (EraseUnchangedGlobalObect(Func, NumFunctions)) {
      Func.deleteBody();
      Func.setMetadata(LLVMContext::MD_fragment, MD);
    }
  }

  DEBUG(dbgs() << "size of module: " << M.size() << '\n');
  DEBUG(dbgs() << "size of removed functions: " << NumFunctions << '\n');
  DEBUG(dbgs() << "size of removed variables: " << NumVariables << '\n');
  DEBUG(dbgs() << "size of removed aliases: " << NumAliases << '\n');

#if 0
  Digest::createDigestFile(M, DigestMap, getPassName());
#endif

  return Changed;
}
