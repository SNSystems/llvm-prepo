//===- lib/MC/RepoObjectWriter.cpp - Program Repository Writer-------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements Program Repository file writer information.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoObjectWriter.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRepoFragment/MCRepoFragment.h"
#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbolRepo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Support/Compression.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/StringSaver.h"
#include <set>
#include <string>
#include <vector>

#include "pstore/transaction.hpp"

using namespace llvm;

#undef DEBUG_TYPE
#define DEBUG_TYPE "reloc-info"

namespace {
typedef DenseMap<const MCSectionRepo *, uint32_t> SectionIndexMapTy;

using TransactionType = pstore::transaction<pstore::transaction_lock>;


class RepoObjectWriter : public MCObjectWriter {
  //  static uint64_t SymbolValue(const MCSymbol &Sym, const MCAsmLayout
  //  &Layout);
  //  static bool isInSymtab(const MCAsmLayout &Layout, const MCSymbolELF
  //  &Symbol,
  //                         bool Used, bool Renamed);

  /// The target specific repository writer instance.
  std::unique_ptr<MCRepoObjectTargetWriter> TargetObjectWriter;

  DenseMap<const MCSymbolRepo *, const MCSymbolRepo *> Renames;

  DenseMap<const MCSectionRepo *, std::vector<RepoRelocationEntry>> Relocations;

  std::map<Digest::DigestType,
           SmallVector<std::unique_ptr<repo::SectionContent>, 4>>
      Contents;

  /// @}
  /// @name Symbol Table Data
  /// @{

  BumpPtrAllocator Alloc;
  StringSaver VersionSymSaver{Alloc};
  // StringTableBuilder StrTabBuilder{StringTableBuilder::ELF};

  /// @}

  // This holds the symbol table index of the last local symbol.
  unsigned LastLocalSymbolIndex;
  // This holds the .strtab section index.
  // unsigned StringTableIndex;
  // This holds the .symtab section index.
  // unsigned SymbolTableIndex;

  // Sections in the order they are to be output in the section table.
  std::vector<const MCSectionRepo *> SectionTable;
  unsigned addToSectionTable(const MCSectionRepo *Sec);

  // TargetObjectWriter wrappers.
  //  bool is64Bit() const { return TargetObjectWriter->is64Bit(); }
  bool hasRelocationAddend() const { return true; }
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const {
    return TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
  }

  void align(unsigned Alignment);

  void writeRepoSectionData(const MCAssembler &Asm, MCSectionRepo &Sec,
                            const MCAsmLayout &Layout,
                            TransactionType & transaction,
                            pstore::index::name_index * const NamesIndex);

  bool shouldRelocateWithSymbol(const MCAssembler &Asm,
                                const MCSymbolRefExpr *RefA, const MCSymbol *S,
                                uint64_t C, unsigned Type) const {
    return true;
  }

public:
  RepoObjectWriter(MCRepoObjectTargetWriter *MOTW, raw_pwrite_stream &OS,
                   bool IsLittleEndian)
      : MCObjectWriter(OS, IsLittleEndian), TargetObjectWriter(MOTW) {}

  void reset() override {
    Renames.clear();
    Relocations.clear();
    // StrTabBuilder.clear();
    SectionTable.clear();
    MCObjectWriter::reset();
  }

  ~RepoObjectWriter() override;

  void WriteWord(uint64_t W) { write64(W); }

  template <typename T> void write(T Val) {
    if (IsLittleEndian)
      support::endian::Writer<support::little>(getStream()).write(Val);
    else
      support::endian::Writer<support::big>(getStream()).write(Val);
  }

  void writeHeader(const MCAssembler &Asm);

  // void writeSymbol(SymbolTableWriter &Writer, uint32_t StringIndex,
  // ELFSymbolData &MSD, const MCAsmLayout &Layout);

  // Start and end offset of each section
  typedef std::map<const MCSectionELF *, std::pair<uint64_t, uint64_t>>
      SectionOffsetsTy;

  void recordRelocation(MCAssembler &Asm, const MCAsmLayout &Layout,
                        const MCFragment *Fragment, const MCFixup &Fixup,
                        MCValue Target, bool &IsPCRel,
                        uint64_t &FixedValue) override;

  // Map from a signature symbol to the group section index
  typedef DenseMap<const MCSymbol *, unsigned> RevGroupMapTy;

  MCSectionRepo *createRelocationSection(MCContext &Ctx,
                                         const MCSectionRepo &Sec);

  // const MCSectionELF *createStringTable(MCContext &Ctx);

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;

#if 0
  void writeSectionHeader(const MCAsmLayout &Layout, const SectionIndexMapTy &SectionIndexMap, const SectionOffsetsTy &SectionOffsets);
#endif
  void writeSectionData(const MCAssembler &Asm, MCSection &Sec,
                        const MCAsmLayout &Layout,
                        TransactionType & Transaction,
                        pstore::index::name_index * const NamesIndex);

#if 0
  void WriteSecHdrEntry(uint32_t Name, uint32_t Type, uint64_t Flags,
                        uint64_t Address, uint64_t Offset, uint64_t Size,
                        uint32_t Link, uint32_t Info, uint64_t Alignment,
                        uint64_t EntrySize);
#endif
  void writeRelocations(const MCAssembler &Asm, const MCSectionRepo &Sec);

  bool isSymbolRefDifferenceFullyResolvedImpl(const MCAssembler &Asm,
                                              const MCSymbol &SymA,
                                              const MCFragment &FB, bool InSet,
                                              bool IsPCRel) const override;

  // bool isWeak(const MCSymbol &Sym) const override;

  void writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
  void writeSection(const SectionIndexMapTy &SectionIndexMap,
                    uint32_t GroupSymbolIndex, uint64_t Offset, uint64_t Size,
                    const MCSectionRepo &Section);
};
} // end anonymous namespace

void RepoObjectWriter::align(unsigned Alignment) {
  uint64_t Padding = OffsetToAlignment(getStream().tell(), Alignment);
  WriteZeros(Padding);
}

unsigned RepoObjectWriter::addToSectionTable(const MCSectionRepo *Sec) {
  SectionTable.push_back(Sec);
  // StrTabBuilder.add(Sec->getSectionName());
  return SectionTable.size();
}

RepoObjectWriter::~RepoObjectWriter() {}

// Emit the ELF header.
void RepoObjectWriter::writeHeader(const MCAssembler &Asm) {
  // ELF Header
  // ----------
  //
  // Note
  // ----
  // emitWord method behaves differently for ELF32 and ELF64, writing
  // 4 bytes in the former and 8 in the latter.

  writeBytes("hello");
}

#if 0
uint64_t RepoObjectWriter::SymbolValue(const MCSymbol &Sym, const MCAsmLayout &Layout) {
  if (Sym.isCommon() && Sym.isExternal())
    return Sym.getCommonAlignment();

  uint64_t Res;
  if (!Layout.getSymbolOffset(Sym, Res))
    return 0;

  if (Layout.getAssembler().isThumbFunc(&Sym))
    Res |= 1;

  return Res;
}
#endif

void RepoObjectWriter::executePostLayoutBinding(MCAssembler &Asm,
                                                const MCAsmLayout &Layout) {
  // Section symbols are used as definitions for undefined symbols with matching
  // names. If there are multiple sections with the same name, the first one is
  // used.
  for (const MCSection &Sec : Asm) {
    const MCSymbol *Begin = Sec.getBeginSymbol();
    if (!Begin)
      continue;

    const MCSymbol *Alias = Asm.getContext().lookupSymbol(Begin->getName());
    if (!Alias || !Alias->isUndefined())
      continue;

    Renames.insert(
        std::make_pair(cast<MCSymbolRepo>(Alias), cast<MCSymbolRepo>(Begin)));
  }
}

#if 0
static uint8_t mergeTypeForSet(uint8_t origType, uint8_t newType) {
  uint8_t Type = newType;

  // Propagation rules:
  // IFUNC > FUNC > OBJECT > NOTYPE
  // TLS_OBJECT > OBJECT > NOTYPE
  //
  // dont let the new type degrade the old type
  switch (origType) {
  default:
    break;
  case ELF::STT_GNU_IFUNC:
    if (Type == ELF::STT_FUNC || Type == ELF::STT_OBJECT ||
        Type == ELF::STT_NOTYPE || Type == ELF::STT_TLS)
      Type = ELF::STT_GNU_IFUNC;
    break;
  case ELF::STT_FUNC:
    if (Type == ELF::STT_OBJECT || Type == ELF::STT_NOTYPE ||
        Type == ELF::STT_TLS)
      Type = ELF::STT_FUNC;
    break;
  case ELF::STT_OBJECT:
    if (Type == ELF::STT_NOTYPE)
      Type = ELF::STT_OBJECT;
    break;
  case ELF::STT_TLS:
    if (Type == ELF::STT_OBJECT || Type == ELF::STT_NOTYPE ||
        Type == ELF::STT_GNU_IFUNC || Type == ELF::STT_FUNC)
      Type = ELF::STT_TLS;
    break;
  }

  return Type;
}
#endif

#if 0
// True if the assembler knows nothing about the final value of the symbol.
// This doesn't cover the comdat issues, since in those cases the assembler
// can at least know that all symbols in the section will move together.
static bool isWeak(const MCSymbolELF &Sym) {
  if (Sym.getType() == ELF::STT_GNU_IFUNC)
    return true;

  switch (Sym.getBinding()) {
  default:
    llvm_unreachable("Unknown binding");
  case ELF::STB_LOCAL:
    return false;
  case ELF::STB_GLOBAL:
    return false;
  case ELF::STB_WEAK:
  case ELF::STB_GNU_UNIQUE:
    return true;
  }
}
#endif

void RepoObjectWriter::recordRelocation(MCAssembler &Asm,
                                        const MCAsmLayout &Layout,
                                        const MCFragment *Fragment,
                                        const MCFixup &Fixup, MCValue Target,
                                        bool &IsPCRel, uint64_t &FixedValue) {
  auto const &FixupSection = cast<MCSectionRepo>(*Fragment->getParent());
  uint64_t C = Target.getConstant();
  uint64_t FixupOffset = Layout.getFragmentOffset(Fragment) + Fixup.getOffset();
  MCContext &Ctx = Asm.getContext();

  if (const MCSymbolRefExpr *RefB = Target.getSymB()) {
    assert(RefB->getKind() == MCSymbolRefExpr::VK_None &&
           "Should not have constructed this");

    // Let A, B and C being the components of Target and R be the location of
    // the fixup. If the fixup is not pcrel, we want to compute (A - B + C).
    // If it is pcrel, we want to compute (A - B + C - R).

    // In general, ELF has no relocations for -B. It can only represent (A + C)
    // or (A + C - R). If B = R + K and the relocation is not pcrel, we can
    // replace B to implement it: (A - R - K + C)
    if (IsPCRel) {
      Ctx.reportError(
          Fixup.getLoc(),
          "No fixup available to represent this relative expression");
      return;
    }

    const auto &SymB = cast<MCSymbolRepo>(RefB->getSymbol());

    if (SymB.isUndefined()) {
      Ctx.reportError(Fixup.getLoc(),
                      Twine("symbol '") + SymB.getName() +
                          "' can not be undefined in a subtraction expression");
      return;
    }

    assert(!SymB.isAbsolute() && "Should have been folded");
    const MCSection &SecB = SymB.getSection();
    if (&SecB != &FixupSection) {
      Ctx.reportError(Fixup.getLoc(),
                      "Cannot represent a difference across sections");
      return;
    }

    uint64_t SymBOffset = Layout.getSymbolOffset(SymB);
    uint64_t K = SymBOffset - FixupOffset;
    IsPCRel = true;
    C -= K;
  }

  // We either rejected the fixup or folded B into C at this point.
  const MCSymbolRefExpr *RefA = Target.getSymA();
  const auto *SymA = RefA ? cast<MCSymbolRepo>(&RefA->getSymbol()) : nullptr;

  bool ViaWeakRef = false;
  if (SymA && SymA->isVariable()) {
    const MCExpr *Expr = SymA->getVariableValue();
    if (const auto *Inner = dyn_cast<MCSymbolRefExpr>(Expr)) {
      if (Inner->getKind() == MCSymbolRefExpr::VK_WEAKREF) {
        SymA = cast<MCSymbolRepo>(&Inner->getSymbol());
        ViaWeakRef = true;
      }
    }
  }

  unsigned Type = getRelocType(Ctx, Target, Fixup, IsPCRel);
  uint64_t OriginalC = C;
  bool RelocateWithSymbol = shouldRelocateWithSymbol(Asm, RefA, SymA, C, Type);
  if (!RelocateWithSymbol && SymA && !SymA->isUndefined()) {
    C += Layout.getSymbolOffset(*SymA);
  }

  uint64_t Addend = 0;
  if (hasRelocationAddend()) {
    Addend = C;
    C = 0;
  }

  FixedValue = C;

  if (!RelocateWithSymbol) {
    const MCSection *SecA =
        (SymA && !SymA->isUndefined()) ? &SymA->getSection() : nullptr;
    auto *ELFSec = cast_or_null<MCSectionRepo>(SecA);
    const MCSymbolRepo *SectionSymbol =
        nullptr; // ELFSec ? cast<MCSymbolRepo>(ELFSec->getBeginSymbol()) :
                 // nullptr;
    if (SectionSymbol) {
      SectionSymbol->setUsedInReloc();
    }
    Relocations[&FixupSection].emplace_back(FixupOffset, SectionSymbol, Type,
                                            Addend, SymA, OriginalC);
    return;
  }

  const auto *RenamedSymA = SymA;
  if (SymA) {
    if (const MCSymbolRepo *R = Renames.lookup(SymA)) {
      RenamedSymA = R;
    }

    // if (ViaWeakRef) {
    //    RenamedSymA->setIsWeakrefUsedInReloc();
    //} else {
    RenamedSymA->setUsedInReloc();
    //}
  }
  Relocations[&FixupSection].emplace_back(FixupOffset, RenamedSymA, Type,
                                          Addend, SymA, OriginalC);
}

#if 0
bool RepoObjectWriter::isInSymtab(const MCAsmLayout &Layout,
                                 const MCSymbolELF &Symbol, bool Used,
                                 bool Renamed) {
  if (Symbol.isVariable()) {
    const MCExpr *Expr = Symbol.getVariableValue();
    if (const MCSymbolRefExpr *Ref = dyn_cast<MCSymbolRefExpr>(Expr)) {
      if (Ref->getKind() == MCSymbolRefExpr::VK_WEAKREF)
        return false;
    }
  }

  if (Used)
    return true;

  if (Renamed)
    return false;

  if (Symbol.isVariable() && Symbol.isUndefined()) {
    // FIXME: this is here just to diagnose the case of a var = commmon_sym.
    Layout.getBaseSymbol(Symbol);
    return false;
  }

  if (Symbol.isUndefined() && !Symbol.isBindingSet())
    return false;

  if (Symbol.isTemporary())
    return false;

  if (Symbol.getType() == ELF::STT_SECTION)
    return false;

  return true;
}
#endif

// createRelocationSection
// ~~~~~~~~~~~~~~~~~~~~~~~
MCSectionRepo *
RepoObjectWriter::createRelocationSection(MCContext &Ctx,
                                          const MCSectionRepo &Sec) {
  return nullptr;

#if 0
  if (Relocations[&Sec].empty())
    return nullptr;

  const StringRef SectionName = Sec.getSectionName();
  std::string RelaSectionName = hasRelocationAddend() ? ".rela" : ".rel";
  RelaSectionName += SectionName;

  unsigned EntrySize;
  if (hasRelocationAddend())
    EntrySize = is64Bit() ? sizeof(ELF::Elf64_Rela) : sizeof(ELF::Elf32_Rela);
  else
    EntrySize = is64Bit() ? sizeof(ELF::Elf64_Rel) : sizeof(ELF::Elf32_Rel);

  unsigned Flags = 0;
  if (Sec.getFlags() & ELF::SHF_GROUP)
    Flags = ELF::SHF_GROUP;

  MCSectionELF *RelaSection = Ctx.createELFRelSection(RelaSectionName, hasRelocationAddend() ? ELF::SHT_RELA : ELF::SHT_REL, Flags, EntrySize, Sec.getGroup(), &Sec);
  RelaSection->setAlignment(is64Bit() ? 8 : 4);
  return RelaSection;
#endif
}

void RepoObjectWriter::writeRepoSectionData(const MCAssembler &Asm,
                                            MCSectionRepo &Section,
                                            const MCAsmLayout &Layout,
                                            TransactionType & Transaction,
                                            pstore::index::name_index * const NamesIndex) {

#if 0
    for (const MCSymbol &S : Asm.symbols()) {
        std::string symbol_name = S.getName();
        if (symbol_name.size () > 0) {
            std::cout << R"(symbol: ")" << symbol_name << "\"\n";

            uint64_t Value;
            bool hasOffset = Layout.getSymbolOffset(S, Value);
            std::cout << " " << hasOffset << " " << Value << '\n';

  Section.getFragmentList().front ();

        }
    }
#endif

  llvm::repo::SectionType St = llvm::repo::SectionType::Data;

  auto const kind = Section.getKind();
  if (kind.isBSS()) {
    St = llvm::repo::SectionType::BSS;
  } else if (kind.isCommon()) {
    St = llvm::repo::SectionType::Common;
  } else if (kind.isData()) {
    St = llvm::repo::SectionType::Data;
  } else if (kind.isReadOnlyWithRel()) {
    St = llvm::repo::SectionType::RelRo;
  } else if (kind.isText()) {
    St = llvm::repo::SectionType::Text;
  } else if (kind.isMergeable1ByteCString()) {
    St = llvm::repo::SectionType::Mergeable1ByteCString;
  } else if (kind.isMergeable2ByteCString()) {
    St = llvm::repo::SectionType::Mergeable2ByteCString;
  } else if (kind.isMergeable4ByteCString()) {
    St = llvm::repo::SectionType::Mergeable4ByteCString;
  } else if (kind.isMergeableConst4()) {
    St = llvm::repo::SectionType::MergeableConst4;
  } else if (kind.isMergeableConst8()) {
    St = llvm::repo::SectionType::MergeableConst8;
  } else if (kind.isMergeableConst16()) {
    St = llvm::repo::SectionType::MergeableConst16;
  } else if (kind.isMergeableConst32()) {
    St = llvm::repo::SectionType::MergeableConst32;
  } else if (kind.isMergeableConst()) {
    St = llvm::repo::SectionType::MergeableConst;
  } else if (kind.isReadOnly()) {
    St = llvm::repo::SectionType::ReadOnly;
  } else if (kind.isThreadBSS()) {
    St = llvm::repo::SectionType::ThreadBSS;
  } else if (kind.isThreadData()) {
    St = llvm::repo::SectionType::ThreadData;
  } else if (kind.isThreadLocal()) {
    St = llvm::repo::SectionType::ThreadLocal;
  } else if (kind.isMetadata()) {
    St = llvm::repo::SectionType::Metadata;
  } else {
    llvm_unreachable("Unknown section type in writeRepoSectionData");
  }

  auto &SC = Contents[Section.hash()];
  SC.push_back(make_unique<repo::SectionContent>(St));
  repo::SectionContent &Content = *SC.back();

  // Add the section content to the fragment.
  raw_svector_ostream VecOS{Content.Data};
  raw_pwrite_stream &OldStream = getStream();
  this->setStream(VecOS);
  Asm.writeSectionData(&Section, Layout);
  this->setStream(OldStream);

  auto const &Relocs = Relocations[&Section];
  Content.Xfixups.reserve(Relocs.size());
  for (auto const &Relocation : Relocations[&Section]) {
    // Insert the target symbol name into the set of known names.
    StringRef Name = Relocation.Symbol->getName();
    auto It = NamesIndex->insert (Transaction, Name).first;

    // Attach a suitable external fixup to this section.
    Content.Xfixups.push_back(repo::ExternalFixup{
        It.get_address (), static_cast<std::uint8_t>(Relocation.Type),
        Relocation.Offset, Relocation.Addend});
  }
}

void RepoObjectWriter::writeSectionData(const MCAssembler &Asm, MCSection &Sec,
                                        const MCAsmLayout &Layout,
                                        TransactionType & Transaction,
                                        pstore::index::name_index * const NamesIndex) {

  auto &Section = static_cast<MCSectionRepo &>(Sec);
  this->writeRepoSectionData(Asm, Section, Layout, Transaction, NamesIndex);
}

#if 0
void RepoObjectWriter::WriteSecHdrEntry(uint32_t Name, uint32_t Type,
                                       uint64_t Flags, uint64_t Address,
                                       uint64_t Offset, uint64_t Size,
                                       uint32_t Link, uint32_t Info,
                                       uint64_t Alignment,
                                       uint64_t EntrySize) {
  write32(Name);        // sh_name: index into string table
  write32(Type);        // sh_type
  WriteWord(Flags);     // sh_flags
  WriteWord(Address);   // sh_addr
  WriteWord(Offset);    // sh_offset
  WriteWord(Size);      // sh_size
  write32(Link);        // sh_link
  write32(Info);        // sh_info
  WriteWord(Alignment); // sh_addralign
  WriteWord(EntrySize); // sh_entsize
}
#endif
void RepoObjectWriter::writeRelocations(const MCAssembler &/*Asm*/,
                                        const MCSectionRepo &/*Sec*/) {
}

#if 0
const MCSectionELF *RepoObjectWriter::createStringTable(MCContext &Ctx) {
  const MCSectionELF *StrtabSection = SectionTable[StringTableIndex - 1];
  getStream() << StrTabBuilder.data();
  return StrtabSection;
}
#endif

void RepoObjectWriter::writeSection(const SectionIndexMapTy &SectionIndexMap,
                                    uint32_t GroupSymbolIndex, uint64_t Offset,
                                    uint64_t Size,
                                    const MCSectionRepo &Section) {
#if 0
#endif
}

namespace {

auto getTransaction () -> std::pair <pstore::database &, TransactionType &> {
    static pstore::database Repository ("./clang.db", true/*writable*/);
    static auto Transaction = pstore::begin (Repository);
    return {Repository, Transaction};
}

raw_ostream & operator<< (raw_ostream & OS, pstore::index::uint128 const & V) {
    auto digitToHex = [] (unsigned v) {
        assert (v < 0x10);
        return static_cast<char> (v + ((v < 10) ? '0' : 'a' - 10));
    };

    std::uint64_t const High = V.high ();
    for (int Shift = 64 - 4; Shift >= 0; Shift -= 4) {
        OS << digitToHex ((High >> Shift) & 0x0F);
    }

    std::uint64_t const Low = V.low ();
    for (int Shift = 64 - 4; Shift >= 0; Shift -= 4) {
        OS <<  digitToHex ((Low >> Shift) & 0x0F);
    }
    return OS;
}

} // (anonymous namespace)


void RepoObjectWriter::writeObject(MCAssembler &Asm,
                                   const MCAsmLayout &Layout) {
  MCContext &Ctx = Asm.getContext();

  writeHeader(Asm);

  // ... then the sections ...
  SectionOffsetsTy SectionOffsets;
  // std::vector<MCSectionELF *> Groups;
  std::vector<MCSectionRepo *> Relocations;

  std::pair <pstore::database &, TransactionType &> DbTransact = getTransaction ();
  auto & Db = DbTransact.first;
  auto & Transaction = DbTransact.second;

  pstore::index::name_index * const NamesIndex = Db.get_name_index ();
  assert (NamesIndex);
  pstore::index::digest_index * const DigestsIndex = Db.get_digest_index ();
  assert (DigestsIndex);

  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Asm, Section, Layout, Transaction, NamesIndex);
  }


  for (auto &Content : Contents) {
    auto const Key = pstore::index::uint128 {Content.first.high (), Content.first.low ()};
    if (DigestsIndex->find (Key) != DigestsIndex->end ()) {
      dbgs () << "fragment " << Key << " exists. skipping\n";
    } else {
      auto Begin = llvm::repo::details::makeSectionContentIterator(Content.second.begin());
      auto End = llvm::repo::details::makeSectionContentIterator(Content.second.end());

      dbgs () << "fragment " << Key << " adding. size=" << repo::Fragment::sizeBytes (Begin, End) << '\n';

      pstore::record FragmentRecord = repo::Fragment::alloc (Transaction, Begin, End);
      auto Kvp = std::make_pair (Key, FragmentRecord);
      DigestsIndex->insert (Transaction, Kvp);
    }
  }

  Transaction.commit ();
}

bool RepoObjectWriter::isSymbolRefDifferenceFullyResolvedImpl(
    const MCAssembler &Asm, const MCSymbol &SA, const MCFragment &FB,
    bool InSet, bool IsPCRel) const {

  const auto &SymA = cast<MCSymbolRepo>(SA);
  if (IsPCRel) {
    assert(!InSet);
    // if (::isWeak(SymA)) {
    //    return false;
    //}
  }
  return MCObjectWriter::isSymbolRefDifferenceFullyResolvedImpl(Asm, SymA, FB,
                                                                InSet, IsPCRel);
}

#if 0
bool RepoObjectWriter::isWeak(const MCSymbol &S) const {
  const auto &Sym = cast<MCSymbolELF>(S);
  if (::isWeak(Sym))
    return true;

  // It is invalid to replace a reference to a global in a comdat
  // with a reference to a local since out of comdat references
  // to a local are forbidden.
  // We could try to return false for more cases, like the reference
  // being in the same comdat or Sym being an alias to another global,
  // but it is not clear if it is worth the effort.
  if (Sym.getBinding() != ELF::STB_GLOBAL)
    return false;

  if (!Sym.isInSection())
    return false;

  const auto &Sec = cast<MCSectionELF>(Sym.getSection());
  return Sec.getGroup();
}
#endif

MCObjectWriter *llvm::createRepoObjectWriter(MCRepoObjectTargetWriter *MOTW,
                                             raw_pwrite_stream &OS,
                                             bool IsLittleEndian) {
  return new RepoObjectWriter(MOTW, OS, IsLittleEndian);
}
