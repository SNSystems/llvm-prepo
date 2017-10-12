//===- llvm/Support/Repo.h - Repo constants and data structures --*- C++-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header contains common, non-processor-specific data structures and
// constants for the REPO file format.
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_REPO_H
#define LLVM_SUPPORT_REPO_H

#include "pstore/uuid.hpp"
#include "llvm/Support/ELF.h"

namespace llvm {

namespace repo {
struct RepoObjectHeader {
  // Object file magic string.
  const char RepoMagic[9] = {'R', 'e', 'p', 'o', 'U', 'u', 'i', 'd', '\0'};
  pstore::uuid uuid;
};

} // end namespace repo

} // end namespace llvm

#endif
