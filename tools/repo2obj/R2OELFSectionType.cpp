//===-- R2OELFSectionType.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "R2OELFSectionType.h"
#include "llvm/Support/raw_ostream.h"

#define X(k)                                                                   \
  case ELFSectionType::k:                                                      \
    OS << #k;                                                                  \
    break;

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST) {
  switch (ST) { LLVM_REPO2OBJ_ELF_SECTION_TYPE }
  return OS;
}
#undef X
