//===-- llvm/MC/MCAsmInfoELF.h - ELF Asm info -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCASMINFOREPO_H
#define LLVM_MC_MCASMINFOREPO_H

#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
class MCAsmInfoRepo : public MCAsmInfo {
  virtual void anchor();
  //MCSection *getNonexecutableStackSection(MCContext &Ctx) const;

protected:
  /// Targets which have non-executable stacks by default can set this to false
  /// to disable the special section which requests a non-executable stack.
  bool UsesNonexecutableStackSection;

  MCAsmInfoRepo();
};
}

#endif // LLVM_MC_MCASMINFOREPO_H

