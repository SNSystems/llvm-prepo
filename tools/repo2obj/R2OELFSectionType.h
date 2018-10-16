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

#define LLVM_REPO2OBJ_ELF_SECTION_TYPE                                         \
  X(text)                                                                      \
  X(bss)                                                                       \
  X(data)                                                                      \
  X(rel_ro)                                                                    \
  X(mergeable_1_byte_c_string)                                                 \
  X(mergeable_2_byte_c_string)                                                 \
  X(mergeable_4_byte_c_string)                                                 \
  X(mergeable_const_4)                                                         \
  X(mergeable_const_8)                                                         \
  X(mergeable_const_16)                                                        \
  X(mergeable_const_32)                                                        \
  X(read_only)                                                                 \
  X(thread_bss)                                                                \
  X(thread_data)                                                               \
  X(debug_line)                                                                \
  X(debug_string)                                                              \
  X(debug_ranges)                                                              \
  X(init_array)                                                                \
  X(fini_array)

#define X(t) t,
enum class ELFSectionType { LLVM_REPO2OBJ_ELF_SECTION_TYPE };
#undef X

namespace llvm {
class raw_ostream;
}
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST);

#endif // LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
