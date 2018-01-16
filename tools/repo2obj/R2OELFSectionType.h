//===-- R2OELFSectionType.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H

#include "pstore_mcrepo/fragment.hpp"

#define X(a) a,
enum class ELFSectionType {
  PSTORE_REPO_SECTION_TYPES InitArray,
  FiniArray,
};
#undef X

#endif // LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
