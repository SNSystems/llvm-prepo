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
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>

using namespace llvm;

namespace llvm {

class MDNode;

void Digest::set(Module const &M, GlobalObject *GO, DigestType const &D) {
  MDBuilder MDB(M.getContext());
  GO->setMetadata(LLVMContext::MD_fragment, MDB.createHashBytes(D));
}

Digest::DigestType Digest::get(const GlobalObject *GO) {
  // Use the digest in the MCSectionRepo.
  MDNode const *MD = GO->getMetadata(LLVMContext::MD_fragment);

  if (!MD) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Failed to get digest metadata for global object '" +
                       GO->getName() + "'.");
  }

  if (MD->getNumOperands() != static_cast<unsigned>(Digest::MDDigest::Last)) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Global object '" + GO->getName() +
                       "' has invalid number of 'digest' metadata operands.");
  }

  MDString const *MDS = dyn_cast<MDString>(
      MD->getOperand(static_cast<unsigned>(Digest::MDDigest::Name)));
  if (!MDS || !MDS->getString().equals("digest")) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Global object '" + GO->getName() +
                       "' has invalid 'digest' string metadata.");
  }

  Constant const *C = mdconst::dyn_extract<Constant>(
      MD->getOperand(static_cast<unsigned>(Digest::MDDigest::Value)));
  if (!C || !C->getType()->isArrayTy()) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error(
        "Global object '" + GO->getName() +
        "' has invalid the digest value type that must be array type.'");
  }

  auto const AarryType = C->getType();
  auto const Elems = AarryType->getArrayNumElements();
  DigestType D;
  if (Elems != D.Bytes.max_size()) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Global object '" + GO->getName() +
                       "' has invalid the digest array size.'");
  }

  if (!AarryType->getArrayElementType()->isIntegerTy(8)) {
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Global object '" + GO->getName() +
                       "' has invalid the array element type which should be "
                       "8-bit integer type.'");
  }

  for (unsigned I = 0, E = Elems; I != E; ++I) {
    ConstantInt const *CI = dyn_cast<ConstantInt>(C->getAggregateElement(I));
    assert(CI);
    D[I] = CI->getValue().getZExtValue();
  }
  return D;
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

void Digest::createHashFile(Module const &M, GlobalValueMap const &GVMap,
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

} // end namespace llvm
