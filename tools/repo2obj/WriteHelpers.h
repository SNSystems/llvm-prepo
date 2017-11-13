//===-- WriteHelpers.h ----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef REPO2OBJ_WRITE_HELPERS_H
#define REPO2OBJ_WRITE_HELPERS_H

#include "llvm/Support/raw_ostream.h"
#include <cstdint>
#include <cstring>
#include <type_traits>

template <typename T, typename = typename std::enable_if<std::is_standard_layout<T>::value>::type>
void zero (T & t) {
    std::memset (&t, 0, sizeof (T));
}

template <typename T, typename = typename std::enable_if<std::is_standard_layout<T>::value>::type>
void writeRaw (llvm::raw_ostream & OS, T const & t) {
    OS.write (reinterpret_cast<char const *> (&t), sizeof (t));
}

template <typename T>
void writeAlignmentPadding (llvm::raw_ostream & OS) {
    constexpr auto Alignment = alignof (T);
    uint8_t Padding[Alignment] = {0};
    OS.write (reinterpret_cast<char const *> (Padding), Alignment - (OS.tell () % Alignment));
}

#endif //REPO2OBJ_WRITE_HELPERS_H
