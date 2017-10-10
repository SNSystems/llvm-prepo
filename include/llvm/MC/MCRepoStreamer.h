//===- MCELFStreamer.h - MCStreamer ELF Object File Interface ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOSTREAMER_H
#define LLVM_MC_MCREPOSTREAMER_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCObjectStreamer.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/Support/DataTypes.h"

namespace llvm {

class MCAsmBackend;
class MCAssembler;
class MCCodeEmitter;
class MCExpr;
class MCInst;
class raw_ostream;

class MCRepoStreamer : public MCObjectStreamer {
public:
  MCRepoStreamer(MCContext &Context, MCAsmBackend &TAB, raw_pwrite_stream &OS,
                 MCCodeEmitter *Emitter)
      : MCObjectStreamer(Context, TAB, OS, Emitter) {}

  ~MCRepoStreamer() override;

  /// state management
  void reset() override {}

  /// \name MCStreamer Interface
  /// @{

  void ChangeSection(MCSection *Section, const MCExpr *Subsection) override;
  bool EmitSymbolAttribute(MCSymbol *Symbol, MCSymbolAttr Attribute) override;
  void EmitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                        unsigned ByteAlignment) override;

  void EmitZerofill(MCSection *Section, MCSymbol *Symbol = nullptr,
                    uint64_t Size = 0, unsigned ByteAlignment = 0) override;

private:
  void EmitInstToData(const MCInst &Inst, const MCSubtargetInfo &) override;
};

} // end namespace llvm

#endif
