//===-- llvm/Support/REPO.h - REPO constants and data structures --*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This header contains common, non-processor-specific data structures and
// constants for the REPO file format.
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_REPO_H
#define LLVM_SUPPORT_REPO_H

#include "llvm/Support/ELF.h"
#include "pstore/uuid.hpp"

namespace llvm {

namespace REPO {
struct RepoObjectHeader {
  // Object file magic string.
  const char RepoMagic[9] = {'R', 'e', 'p', 'o', 'U', 'u', 'i', 'd', '\0'};
  pstore::uuid uuid;
};

} // end namespace REPO

} // end namespace llvm

#endif
