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

//#include "llvm/ADT/Twine.h"
#include "llvm/IR/Digest.h"
#include "llvm/MC/MCSection.h"
//#include "llvm/MC/MCSymbolELF.h"
//#include "llvm/Support/Debug.h"
//#include "llvm/Support/ELF.h"
#include "llvm/Support/MD5.h"
//#include "llvm/Support/raw_ostream.h"

namespace llvm {

// class MCSymbol;

class MCSectionRepo : public MCSection {
private:
  std::string id_;
  Digest::DigestType digest_;
  unsigned const index_;

  friend class MCContext;
  MCSectionRepo(SectionKind K, MCSymbol *Begin);
  MCSectionRepo(SectionKind K, MCSymbol *Begin, std::string id,
                Digest::DigestType digest);

  void PrintSwitchToSection(const MCAsmInfo &MAI, const Triple &T,
                            raw_ostream &OS,
                            const MCExpr *Subsection) const override {}

  bool UseCodeAlign() const override { return false; }
  bool isVirtualSection() const override { return false; }

  // void setSectionName(StringRef Name) { SectionName = Name; }

public:
  std::string id() const { return id_; }
  Digest::DigestType hash() const { return digest_; }
  virtual ~MCSectionRepo();

  static bool classof(const MCSection *S) { return S->getVariant() == SV_Repo; }
};

} // end namespace llvm

#endif
