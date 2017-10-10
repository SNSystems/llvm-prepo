//===- MCSymbolRepo.h -  ---------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
#ifndef LLVM_MC_MCSYMBOLREPO_H
#define LLVM_MC_MCSYMBOLREPO_H

#include "llvm/MC/MCSymbol.h"

namespace llvm {

class MCSymbolRepo : public MCSymbol {

public:
  MCSymbolRepo(const StringMapEntry<bool> *Name, bool isTemporary)
      : MCSymbol(SymbolKindRepo, Name, isTemporary) {}

  static bool classof(const MCSymbol *S) { return S->isRepo(); }
};

} // end namespace llvm

#endif
