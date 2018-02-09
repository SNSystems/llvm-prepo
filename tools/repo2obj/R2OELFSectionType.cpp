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

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST) {
  switch (ST) {
  case ELFSectionType::text:
    OS << "text";
    break;
  case ELFSectionType::bss:
    OS << "bss";
    break;
  case ELFSectionType::data:
    OS << "data";
    break;
  case ELFSectionType::rel_ro:
    OS << "rel_ro";
    break;
  case ELFSectionType::mergeable_1_byte_c_string:
    OS << "mergeable_1_byte_c_string";
    break;
  case ELFSectionType::mergeable_2_byte_c_string:
    OS << "mergeable_2_byte_c_string";
    break;
  case ELFSectionType::mergeable_4_byte_c_string:
    OS << "mergeable_4_byte_c_string";
    break;
  case ELFSectionType::mergeable_const_4:
    OS << "mergeable_const_4";
    break;
  case ELFSectionType::mergeable_const_8:
    OS << "mergeable_const_8";
    break;
  case ELFSectionType::mergeable_const_16:
    OS << "mergeable_const_16";
    break;
  case ELFSectionType::mergeable_const_32:
    OS << "mergeable_const_32";
    break;
  case ELFSectionType::read_only:
    OS << "read_only";
    break;
  case ELFSectionType::thread_bss:
    OS << "thread_bss";
    break;
  case ELFSectionType::thread_data:
    OS << "thread_data";
    break;
  case ELFSectionType::init_array:
    OS << "init_array";
    break;
  case ELFSectionType::fini_array:
    OS << "fini_array";
    break;
  }
  return OS;
}
