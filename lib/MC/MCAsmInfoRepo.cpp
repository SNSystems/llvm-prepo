//===-- MCAsmInfoELF.cpp - ELF asm properties -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines target asm properties related what form asm statements
// should take in general on ELF-based targets
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCAsmInfoRepo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionRepo.h"
//#include "llvm/Support/ELF.h"
using namespace llvm;

void MCAsmInfoRepo::anchor() { }

//MCSection *MCAsmInfoRepo::getNonexecutableStackSection(MCContext &Ctx) const {
//    return nullptr;
//}

MCAsmInfoRepo::MCAsmInfoRepo() {
  HasIdentDirective = false;
  WeakRefDirective = "\t.weak\t";
  PrivateGlobalPrefix = ".L";
  PrivateLabelPrefix = ".L";
  UsesNonexecutableStackSection = false;
  HasDotTypeDotSizeDirective = false;
}

