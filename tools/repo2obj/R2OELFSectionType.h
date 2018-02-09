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

// clang-format off
#define X(a) a,
enum class ELFSectionType {
  PSTORE_REPO_SECTION_TYPES
  init_array,
  fini_array,
};
#undef X
// clang-format on

namespace llvm {
class raw_ostream;
}
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST);

#endif // LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
