//===-- MCAsmInfoRepo.cpp - Repo asm properties -----------------*- C++ -*-===//
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
using namespace llvm;

void MCAsmInfoRepo::anchor() {}

MCAsmInfoRepo::MCAsmInfoRepo() {
  // Repo uses the MM_None mangling mode type.
  HasIdentDirective = false;
  WeakRefDirective = "\t.weak\t";
  PrivateGlobalPrefix = "";
  PrivateLabelPrefix = ".L";
  HasDotTypeDotSizeDirective = false;

  // Set up DWARF directives
  SupportsDebugInformation = true;
  NeedsDwarfSectionOffsetDirective = true;
}
