//===-- Digest.h - Program repository digest data structure. ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the digest data structure.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_IR_DIGEST_H
#define LLVM_IR_DIGEST_H

#include "llvm/IR/GlobalValue.h"
#include "llvm/Support/MD5.h"
#include <map>

namespace llvm {
class GlobalObject;

struct Digest {
  using DigestType = MD5::MD5Result;
  using GlobalValueMap = std::map<const GlobalValue *, const DigestType>;
  enum class MDDigest { Name, Value, Last };
  static const Constant *getAliasee(const GlobalAlias *GA);
  // Create a hash file for debugging purpose.
  static void createHashFile(Module const &M, GlobalValueMap const &ObjMap,
                             StringRef FileExt);

  /// \brief Get global object digest metadata value.
  static void set(Module const &M, GlobalObject *GO, DigestType const &D);
  /// \brief Get global object digest metadata value.
  static DigestType get(const GlobalObject *GO);
};

} // end namespace llvm

#endif // LLVM_IR_DIGEST_H
