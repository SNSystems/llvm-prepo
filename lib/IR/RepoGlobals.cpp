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
    // FIXME: this should be coming from the command-line!
    char const * path = getenv ("REPOFILE");
    if (!path) {
        path = "./clang.db";
    }

    Repository.reset(new pstore::database(
        path, pstore::database::access_mode::writable));
  }
  return *Repository;
}

