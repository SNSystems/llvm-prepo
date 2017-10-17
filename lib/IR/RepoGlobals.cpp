//===----  RepoGlobals.cpp - Program repository globals  ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoGlobals.h"
#include "pstore/transaction.hpp"

using namespace llvm;

pstore::database &llvm::getRepoDatabase() {
  static std::unique_ptr<pstore::database> Repository;
  if (!Repository) {
    // FIME: the name of the store is just hard-wired for the moment.
    Repository.reset(new pstore::database(
        "./clang.db", pstore::database::access_mode::writable));
  }
  return *Repository;
}

