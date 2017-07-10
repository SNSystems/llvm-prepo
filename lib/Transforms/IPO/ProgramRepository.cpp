//===- ProgramRepository.cpp - Create a program repository ----------------===//
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
#include "llvm/IR/Digest.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/HashCalculator.h"

using namespace llvm;

#define DEBUG_TYPE "prepo"

STATISTIC(NumFunctions, "Number of functions hashed");
STATISTIC(NumVariables, "Number of variables hashed");
STATISTIC(NumAliases, "Number of aliases hashed");

namespace {

/// ProgramRepository finds functions, gloabal variables and calculate the hash
/// values.
class ProgramRepository : public ModulePass {
public:
  static char ID;
  ProgramRepository() : ModulePass(ID), HasGlobalAliases(false) {
    initializeProgramRepositoryPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override { return "PrepoDigestPass"; }

  bool runOnModule(Module &M) override;

private:
  /// Whether or not the target supports global aliases.
  bool HasGlobalAliases;

  bool isObjFormatRepo(Module &M) const {
    return Triple(M.getTargetTriple()).isOSBinFormatRepo();
  }
};

} // end anonymous namespace

char ProgramRepository::ID = 0;
INITIALIZE_PASS(ProgramRepository, "prepo", "Create Program Repository", false,
                false)

ModulePass *llvm::createProgramRepositoryPass() {
  return new ProgramRepository();
}

using GlobalValueMap = Digest::GlobalValueMap;
namespace {

template <typename T> // primary template
struct DigestCalculator {};

template <> // explicit specialization for T = GlobalVariable
struct DigestCalculator<GlobalVariable> {
  using Calculator = VaribleHashCalculator;
};

template <> // explicit specialization for T = Function
struct DigestCalculator<Function> {
  using Calculator = FunctionHashCalculator;
};
} // namespace

template <typename T>
static void setMetadata(Module &M, T &GO, GlobalValueMap &DigestMap,
                        bool &Changed, llvm::Statistic &Num) {
  if (GO.isDeclaration() || GO.hasAvailableExternallyLinkage())
    return;
  // Calculate the global object hash value.
  typename DigestCalculator<T>::Calculator GOHC{&GO};
  GOHC.calculateHash(M);
  Digest::DigestType Result = GOHC.getHashResult();
  DigestMap.emplace(&GO, Result);
  Digest::set(M, &GO, Result);
  Changed = true;
  ++Num;
}

bool ProgramRepository::runOnModule(Module &M) {
  if (skipModule(M) || !isObjFormatRepo(M))
    return false;

  bool Changed = false;
  MDBuilder MDB(M.getContext());

  GlobalValueMap DigestMap;

  for (GlobalVariable &GV : M.globals()) {
    setMetadata<GlobalVariable>(M, GV, DigestMap, Changed, NumVariables);
  }

  for (Function &Func : M) {
    setMetadata<Function>(M, Func, DigestMap, Changed, NumFunctions);
  }

  for (GlobalAlias &GA : M.aliases()) {
    auto GAAliasee = dyn_cast<GlobalValue>(Digest::getAliasee(&GA));
    auto GADigest = DigestMap[GAAliasee];
    DigestMap.emplace(&GA, GADigest);
    Changed = true;
    ++NumAliases;
  }

  DEBUG(dbgs() << "size of module: " << M.size() << '\n');
  DEBUG(dbgs() << "size of hashed functions: " << NumFunctions << '\n');
  DEBUG(dbgs() << "size of hashed variables: " << NumVariables << '\n');
  DEBUG(dbgs() << "size of hashed aliases: " << NumAliases << '\n');

#if 0
  Digest::createDigestFile(M, DigestMap, getPassName());
#endif

  return true;
}
