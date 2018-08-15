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

#include "pstore/mcrepo/fragment.hpp"

enum class ELFSectionType {
  text,
  bss,
  data,
  rel_ro,
  mergeable_1_byte_c_string,
  mergeable_2_byte_c_string,
  mergeable_4_byte_c_string,
  mergeable_const_4,
  mergeable_const_8,
  mergeable_const_16,
  mergeable_const_32,
  read_only,
  thread_bss,
  thread_data,

  init_array,
  fini_array,
};

namespace llvm {
class raw_ostream;
}
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST);

#endif // LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
