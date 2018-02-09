//===- MCSectionRepo.h - Repository Machine Code Sections -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the MCSectionRepo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCSECTIONREPO_H
#define LLVM_MC_MCSECTIONREPO_H

#include "llvm/IR/RepoTicket.h"
#include "llvm/MC/MCSection.h"
#include "llvm/Support/MD5.h"

namespace llvm {

class MCSectionRepo : public MCSection {
private:
  ::llvm::Digest::DigestType Digest;
  /// Monotonically increases for each section.
  unsigned const Index;
  /// A dummy section is created for the assembler's initial setup. Is this the
  /// dummy section?
  bool IsDummy = false;

  friend class MCContext;
  MCSectionRepo(SectionKind K, MCSymbol *Begin);
  MCSectionRepo(SectionKind K, MCSymbol *Begin, Digest::DigestType digest);

  void PrintSwitchToSection(const MCAsmInfo &MAI, const Triple &T,
                            raw_ostream &OS,
                            const MCExpr *Subsection) const override {}

  bool UseCodeAlign() const override { return false; }
  bool isVirtualSection() const override { return false; }

public:
  virtual ~MCSectionRepo();

  void markAsDummy () { IsDummy = true; }
  bool isDummy () const { return IsDummy; }
  Digest::DigestType hash() const { return Digest; }

  static bool classof(const MCSection *S) { return S->getVariant() == SV_Repo; }
};

} // end namespace llvm

#endif
