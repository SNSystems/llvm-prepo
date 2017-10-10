//===- lib/MC/MCRepoStreamer.cpp - Repo Object Output ------------------- -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------- ------===//
//
// This file assembles .s files and emits ELF .o object files.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCRepoStreamer.h"
#include "llvm/Support/TargetRegistry.h"
#include <iostream>

using namespace llvm;
MCRepoStreamer::~MCRepoStreamer() {}

void MCRepoStreamer::ChangeSection(MCSection *Section,
                                   const MCExpr *Subsection) {
  MCSymbol *const beginSymbol = Section->getBeginSymbol();
  std::string name =
      (beginSymbol == nullptr) ? "<none>" : beginSymbol->getName();
  std::cout << "Repo change section " << name << "\n";

  this->MCObjectStreamer::ChangeSection(Section, Subsection);
}

bool MCRepoStreamer::EmitSymbolAttribute(MCSymbol *Symbol,
                                         MCSymbolAttr Attribute) {
  return true; // success
}

void MCRepoStreamer::EmitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                                      unsigned ByteAlignment) {
  std::cout << "Emit common symbol\n";
}

void MCRepoStreamer::EmitZerofill(MCSection *Section, MCSymbol *Symbol,
                                  uint64_t Size, unsigned ByteAlignment) {
  std::cout << "Emit zero fill\n";
}

void MCRepoStreamer::EmitInstToData(const MCInst &Inst,
                                    const MCSubtargetInfo &STI) {
  MCDataFragment *const DF = this->getOrCreateDataFragment();

  SmallVector<MCFixup, 4> Fixups;
  SmallString<256> Code;
  raw_svector_ostream VecOS(Code);
  getAssembler().getEmitter().encodeInstruction(Inst, VecOS, Fixups, STI);

  // Add the fixups and data.
  for (MCFixup &Fixup : Fixups) {
    Fixup.setOffset(Fixup.getOffset() + DF->getContents().size());
    DF->getFixups().push_back(Fixup);
  }
  DF->getContents().append(Code.begin(), Code.end());
}

MCStreamer *llvm::createRepoStreamer(MCContext &Context, MCAsmBackend &MAB,
                                     raw_pwrite_stream &OS, MCCodeEmitter *CE) {

  MCRepoStreamer *S = new MCRepoStreamer(Context, MAB, OS, CE);
  return S;
}
