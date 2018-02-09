//===-- WriteHelpers.cpp --------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "WriteHelpers.h"

std::size_t write8(llvm::raw_ostream &OS, std::uint8_t V) {
  assert(OS.tell() % alignof(decltype(V)) == 0);
  OS.write(reinterpret_cast<char const *>(&V), sizeof(V));
  return sizeof(V);
}
