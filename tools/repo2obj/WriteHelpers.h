//===-- WriteHelpers.h ----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_REPO2OBJ_WRITEHELPERS_H
#define LLVM_TOOLS_REPO2OBJ_WRITEHELPERS_H

#include "llvm/Support/raw_ostream.h"
#include <cstdint>
#include <cstring>
#include <type_traits>

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
void zero(Ty &T) {
  std::memset(&T, 0, sizeof(T));
}

template <typename Ty> void writeAlignmentPadding(llvm::raw_ostream &OS) {
  constexpr auto Alignment = alignof(Ty);
  uint8_t Padding[Alignment] = {0};
  OS.write(reinterpret_cast<char const *>(Padding),
           Alignment - (OS.tell() % Alignment));
}

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty const &T) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), sizeof(T));
  return sizeof(T);
}

std::size_t write8(llvm::raw_ostream &OS, std::uint8_t V);

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty *T, std::size_t Size) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), Size);
  return Size;
}

#endif // LLVM_TOOLS_REPO2OBJ_WRITEHELPERS_H
