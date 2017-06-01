//===- ProgramRepository.cpp - Create a program repository ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
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
};

} // end anonymous namespace

char ProgramRepository::ID = 0;
INITIALIZE_PASS(ProgramRepository, "prepo", "Create Program Repository", false,
                false)

ModulePass *llvm::createProgramRepositoryPass() {
  return new ProgramRepository();
}

static void printHash(raw_fd_ostream &OS, StringRef Name,
                      HashBytesType &Bytes) {
  OS << '\t' << Name << ": HEX : 0x";
  for (int i = 0; i < 16; ++i)
    OS << format("%.2x", Bytes[i]);
  OS << '\n';
}

bool ProgramRepository::runOnModule(Module &M) {
  if (skipModule(M))
    return false;

#ifndef NDEBUG
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
#endif

  MDBuilder MDB(M.getContext());
  std::map<GlobalVariable *, HashType> HashedGVs;
  HashBytesType Bytes;

#ifndef NDEBUG
  OS << "The summary of global variables hash:\n";
#endif

  for (GlobalVariable &G : M.globals()) {
    //    if (!G.hasName())
    //      continue;
    auto GVHC = VaribleHashCalculator(&G);
    GVHC.calculateVaribleHash(M);
    MD5::MD5Result Result;
    GVHC.getHashResult(Result);
    HashedGVs[&G] = Result.words();
    Bytes = Result;
    if (M.getDataLayout().isLittleEndian())
      std::reverse(Bytes.begin(), Bytes.end());
    G.setMetadata(LLVMContext::MD_fragment, MDB.createHashBytes(Bytes));

    ++NumVariablesHashed;

#ifndef NDEBUG
    printHash(OS, G.getName(), Bytes);
#endif
  }

  // All functions in the module, ordered by hash. Functions with a unique
  // hash value are easily eliminated.

  std::map<Function *, HashType> HashedFuncs;

#ifndef NDEBUG
  OS << "The summary of function hash:\n";
#endif
  for (Function &Func : M) {
    if (Func.isDeclaration() || Func.hasAvailableExternallyLinkage())
      continue;
    auto GFHC = FunctionHashCalculator(&Func, &HashedGVs);
    GFHC.calculateFunctionHash(M);
    MD5::MD5Result Result;
    GFHC.getHashResult(Result);
    HashedFuncs[&Func] = Result.words();
    Bytes = Result;
    if (M.getDataLayout().isLittleEndian())
      std::reverse(Bytes.begin(), Bytes.end());
    Func.setMetadata(LLVMContext::MD_fragment, MDB.createHashBytes(Bytes));

    ++NumFunctionsHashed;

#ifndef NDEBUG
    printHash(OS, Func.getName(), Bytes);
#endif
  }

  DEBUG(dbgs() << "size of module: " << M.size() << '\n');
  DEBUG(dbgs() << "size of HashedFuncs: " << HashedFuncs.size() << '\n');

#ifndef NDEBUG
  OS << "The summary of global alias hash:\n";
#endif

  for (GlobalAlias &A : M.aliases()) {
    auto Aliasee = A.getAliasee();
    assert(Aliasee && "Aliasee cannot be NULL!");

    auto Target = Aliasee->stripPointerCasts();
    assert(Target && "Target cannot be NULL!");
    // After stripping pointer casts, the target type shoulf be only GlobalValue
    // type.
    assert(isa<GlobalValue>(Target) && "Aliasee should be only GlobalValue");

#ifndef NDEBUG
    OS << '\t' << A.getName() << ": Alias to ";
    if (auto GVA = dyn_cast<GlobalVariable>(Target)) {
      OS << "a global variable: " << GVA->getName() << ": 0x"
         << utohexstr(HashedGVs[GVA].first) << utohexstr(HashedGVs[GVA].second)
         << '\n';
    } else if (auto GVF = dyn_cast<Function>(Target)) {
      OS << "a function: " << GVF->getName() << ": 0x"
         << utohexstr(HashedFuncs[GVF].first)
         << utohexstr(HashedFuncs[GVF].second) << '\n';
    } else {
      assert(false && "alias to unknown GlobalValue type!");
    }
#endif
    ++NumAliasesHashed;
  }

  return true;
}
