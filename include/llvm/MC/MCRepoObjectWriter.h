//===-- llvm/MC/MCRepoObjectWriter.h - Repository Writer --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOOBJECTWRITER_H
#define LLVM_MC_MCREPOOBJECTWRITER_H

#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

namespace llvm {
class MCAssembler;
class MCContext;
class MCFixup;
class MCFragment;
class MCObjectWriter;
class MCSymbol;
class MCSymbolRepo;
class MCValue;
class raw_pwrite_stream;

struct RepoRelocationEntry {
  uint64_t Offset;            // Where is the relocation.
  const MCSymbolRepo *Symbol; // The symbol to relocate with.
  unsigned Type;              // The type of the relocation.
  uint64_t Addend;            // The addend to use.
  const MCSymbolRepo
      *OriginalSymbol;     // The original value of Symbol if we changed it.
  uint64_t OriginalAddend; // The original value of addend.

  RepoRelocationEntry(uint64_t Offset, const MCSymbolRepo *Symbol,
                      unsigned Type, uint64_t Addend,
                      const MCSymbolRepo *OriginalSymbol,
                      uint64_t OriginalAddend)
      : Offset(Offset), Symbol(Symbol), Type(Type), Addend(Addend),
        OriginalSymbol(OriginalSymbol), OriginalAddend(OriginalAddend) {}

  void print(raw_ostream &Out) const {
    Out << "Off=" << Offset << ", Sym=" << Symbol << ", Type=" << Type
        << ", Addend=" << Addend << ", OriginalSymbol=" << OriginalSymbol
        << ", OriginalAddend=" << OriginalAddend;
  }
  void dump() const { print(errs()); }
};

class MCRepoObjectTargetWriter {
  const uint16_t EMachine;
  virtual void anchor();

protected:
  MCRepoObjectTargetWriter(uint16_t EM);

public:
  virtual ~MCRepoObjectTargetWriter() {}

  virtual unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                                const MCFixup &Fixup, bool IsPCRel) const = 0;

  virtual bool needsRelocateWithSymbol(const MCSymbol &Sym,
                                       unsigned Type) const;

  virtual void sortRelocs(const MCAssembler &Asm,
                          std::vector<RepoRelocationEntry> &Relocs);

  /// \name Accessors
  /// @{
  uint16_t getEMachine() const { return EMachine; }
  /// @}
};

/// \brief Construct a new Repository writer instance.
///
/// \param MOTW - The target specific writer subclass.
/// \param OS - The stream to write to.
/// \returns The constructed object writer.
std::unique_ptr<MCObjectWriter>
createRepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                       raw_pwrite_stream &OS, bool IsLittleEndian);
} // namespace llvm

#endif // LLVM_MC_MCREPOOBJECTWRITER_H
