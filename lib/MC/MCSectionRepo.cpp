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
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSymbol.h"

using namespace llvm;

namespace {
unsigned idx = 0;
}

MCSectionRepo::MCSectionRepo(SectionKind K, DebugSectionKind DK,
                             MCSymbol *Begin)
    : MCSection(SV_Repo, K, Begin), DebugKind{DK}, Index{++idx} {

  assert((K.isMetadata() && DK != DebugSectionKind::None) ||
         (!K.isMetadata() && DK == DebugSectionKind::None));
}

MCSectionRepo::MCSectionRepo(SectionKind K, DebugSectionKind DK,
                             MCSymbol *Begin, ticketmd::DigestType Digest)
    : MCSection(SV_Repo, K, Begin), DebugKind{DK}, Digest{std::move(Digest)},
      Index{++idx} {

  assert((K.isMetadata() && DK != DebugSectionKind::None) ||
         (!K.isMetadata() && DK == DebugSectionKind::None));
}

MCSectionRepo::~MCSectionRepo() {}

MCSection *MCSectionRepo::associatedDebugLineSection(MCContext &Ctx) {
  return Ctx.getRepoSection(MCContext::RepoSection::DebugLine, Digest);
}
