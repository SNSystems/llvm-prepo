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

STATISTIC(NumFunctionsHashed, "Number of functions hashed");
STATISTIC(NumVariablesHashed, "Number of variables hashed");
STATISTIC(NumAliasesHashed, "Number of aliases hashed");

namespace {

/// ProgramRepository finds functions, gloabal variables and calculate the hash
/// values.
class ProgramRepository : public ModulePass {
public:
  static char ID;
  ProgramRepository() : ModulePass(ID), HasGlobalAliases(false) {
    initializeProgramRepositoryPass(*PassRegistry::getPassRegistry());
  }

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

using GlobalVariableMap = std::map<const GlobalVariable *, HashType>;
using GlobalFunctionMap = std::map<const Function *, HashType>;
using GlobalAliasMap = std::map<const GlobalAlias *, Constant *>;

static void createHashFile(Module &M, GlobalVariableMap &VarMap,
                           GlobalFunctionMap &FuncMap,
                           GlobalAliasMap &GAliases) {
  const std::string &MId = M.getModuleIdentifier();
  std::string hashFName(MId.begin(), find(MId, '.'));
  hashFName += ".hash";
  std::error_code EC;
  raw_fd_ostream OS(hashFName, EC, sys::fs::F_Text);
  if (EC) {
    errs() << "Couldn't open " << M.getModuleIdentifier()
           << " for generating hash.\nError:" << EC.message() << "\n";
    exit(1);
  }

  if (!VarMap.empty()) {
    OS << "The summary of global variables hash:\n";
    for (auto const &GV : VarMap) {
      OS << '\t' << GV.first->getName() << ": " << GV.second.digest() << '\n';
    }
  }

  if (!FuncMap.empty()) {
    OS << "The summary of function hash:\n";
    for (auto const &GF : FuncMap) {
      OS << '\t' << GF.first->getName() << ": " << GF.second.digest() << '\n';
    }
  }

  if (!GAliases.empty()) {
    OS << "The summary of global alias hash:\n";
    for (auto const &GA : GAliases) {
      OS << '\t' << GA.first->getName() << ": Alias to ";
      if (auto GVA = dyn_cast<GlobalVariable>(GA.second)) {
        OS << "a global variable: " << GVA->getName() << ": "
           << VarMap[GVA].digest() << '\n';
      } else if (auto GVF = dyn_cast<Function>(GA.second)) {
        OS << "a function: " << GVF->getName() << ": " << FuncMap[GVF].digest()
           << '\n';
      } else {
        assert(false && "alias to unknown GlobalValue type!");
      }
    }
  }
}

bool ProgramRepository::runOnModule(Module &M) {
  if (skipModule(M) || !isObjFormatRepo(M))
    return false;

  MDBuilder MDB(M.getContext());

  GlobalVariableMap HashedGVs;
  for (GlobalVariable &GV : M.globals()) {
    if (GV.isDeclaration())
      continue;
    auto GVHC = VaribleHashCalculator(&GV);
    GVHC.calculateVaribleHash(M);
    MD5::MD5Result Result;
    GVHC.getHashResult(Result);
    HashedGVs[&GV] = Result;
    GV.setMetadata(LLVMContext::MD_fragment, MDB.createHashBytes(Result));
    ++NumVariablesHashed;
  }

  GlobalFunctionMap HashedFuncs;
  for (Function &Func : M) {
    if (Func.isDeclaration() || Func.hasAvailableExternallyLinkage())
      continue;
    auto GFHC = FunctionHashCalculator(&Func);
    GFHC.calculateFunctionHash(M);
    MD5::MD5Result Result;
    GFHC.getHashResult(Result);
    HashedFuncs[&Func] = Result;
    Func.setMetadata(LLVMContext::MD_fragment, MDB.createHashBytes(Result));
    ++NumFunctionsHashed;
  }

  DEBUG(dbgs() << "size of module: " << M.size() << '\n');
  DEBUG(dbgs() << "size of HashedFuncs: " << HashedFuncs.size() << '\n');

  GlobalAliasMap AliveAliases;
  for (GlobalAlias &GA : M.aliases()) {
    auto Aliasee = GA.getAliasee();
    assert(Aliasee && "Aliasee cannot be NULL!");
    auto Target = Aliasee->stripPointerCasts();
    assert(Target && "Target cannot be NULL!");
    // After stripping pointer casts, the target type should be only GlobalValue
    // type.
    assert(isa<GlobalValue>(Target) && "Aliasee should be only GlobalValue");
    AliveAliases[&GA] = Target;
    ++NumAliasesHashed;
  }

#ifdef DEBUG
  createHashFile(M, HashedGVs, HashedFuncs, AliveAliases);
#endif

  return true;
}
