//===- MCSectionRepo.cpp - Repo Code Section Representation -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the MCSectionRepo class.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbol.h"

using namespace llvm;

namespace {
unsigned idx = 0;
}

MCSectionRepo::MCSectionRepo(SectionKind K, MCSymbol *Begin)
    : MCSection(SV_Repo, K, Begin), Index{++idx} {}

MCSectionRepo::MCSectionRepo(SectionKind K, MCSymbol *Begin,
                             ticketmd::DigestType Digest)
    : MCSection(SV_Repo, K, Begin), Digest{std::move(Digest)}, Index{++idx} {}

MCSectionRepo::~MCSectionRepo() {}
