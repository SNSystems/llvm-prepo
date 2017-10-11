//===-- llvm/MC/MCAsmInfoRepo.h - Repo Asm info -----------------*- C++ -*-===//
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

protected:
  MCAsmInfoRepo();
};
} // namespace llvm

#endif // LLVM_MC_MCASMINFOREPO_H
