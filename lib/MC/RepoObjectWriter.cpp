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
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"

#include "pstore_support/portab.hpp"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbolRepo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Support/Compression.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/StringSaver.h"
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "pstore/transaction.hpp"
#include "pstore_mcrepo/Fragment.h"
#include "pstore_mcrepo/ticket.h"

using namespace llvm;

#undef DEBUG_TYPE
#define DEBUG_TYPE "reloc-info"

namespace {
typedef DenseMap<const MCSectionRepo *, uint32_t> SectionIndexMapTy;

using TransactionType = pstore::transaction<pstore::transaction_lock>;

auto StringHash = [](StringRef s) { return HashString(s); };

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

  // Note that I don't use StringMap because we take pointers into this
  // structure that must survive insertion.
  using ModuleNamesContainer =
      std::unordered_map<StringRef, pstore::address, decltype(StringHash)>;

  std::map<Digest::DigestType,
           SmallVector<std::unique_ptr<pstore::repo::SectionContent>, 4>>
      Contents;

  std::map<pstore::uuid, std::vector<pstore::repo::ticket_member>>
      TicketContents;

  /// @}
  /// @name Symbol Table Data
  /// @{

  repo::RepoObjectHeader Header;

  StringRef OutputFile;

  BumpPtrAllocator Alloc;
  StringSaver VersionSymSaver{Alloc};
  // StringTableBuilder StrTabBuilder{StringTableBuilder::ELF};

  /// @}

  // TargetObjectWriter wrappers.
  bool hasRelocationAddend() const { return true; }
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const {
    return TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
  }

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

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;

  void writeSectionData(const MCAssembler &Asm, MCSection &Sec,
                        const MCAsmLayout &Layout, ModuleNamesContainer &Names);

  void writeTicketNodes(const MCAssembler &Asm, ModuleNamesContainer &Names);

  bool isSymbolRefDifferenceFullyResolvedImpl(const MCAssembler &Asm,
                                              const MCSymbol &SymA,
                                              const MCFragment &FB, bool InSet,
                                              bool IsPCRel) const override;

  // bool isWeak(const MCSymbol &Sym) const override;

  void writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
};
} // end anonymous namespace

RepoObjectWriter::~RepoObjectWriter() {}

// Emit the ELF header.
void RepoObjectWriter::writeHeader(const MCAssembler &Asm) {
  writeBytes(Header.RepoMagic);
  writeBytes(
      StringRef(reinterpret_cast<const char *>(Header.uuid.array().data()),
                Header.uuid.elements));
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

namespace {
/// A raw_ostream that writes to an SmallVector or SmallString.  This is a
/// simple adaptor class. This class does not encounter output errors.
/// raw_svector_ostream operates without a buffer, delegating all memory
/// management to the SmallString. Thus the SmallString is always up-to-date,
/// may be used directly and there is no need to call flush().
template <typename Container> class svector_ostream : public raw_pwrite_stream {
public:
  /// Construct a new raw_svector_ostream.
  ///
  /// \param O The vector to write to; this should generally have at least 128
  /// bytes free to avoid any extraneous memory overhead.
  explicit svector_ostream(Container &O) : OS_(O) { SetUnbuffered(); }

  ~svector_ostream() override = default;

  void flush() = delete;

  /// Return a StringRef for the vector contents.
  StringRef str() { return StringRef(OS_.data(), OS_.size()); }

private:
  Container &OS_;

  /// See raw_ostream::write_impl.
  void write_impl(const char *Ptr, size_t Size) override;

  void pwrite_impl(const char *Ptr, size_t Size, uint64_t Offset) override;

  /// Return the current position within the stream.
  uint64_t current_pos() const override;
};

template <typename Container>
uint64_t svector_ostream<Container>::current_pos() const {
  return OS_.size();
}

template <typename Container>
void svector_ostream<Container>::write_impl(const char *Ptr, size_t Size) {
  OS_.append(Ptr, Ptr + Size);
}

template <typename Container>
void svector_ostream<Container>::pwrite_impl(const char *Ptr, size_t Size,
                                             uint64_t Offset) {
  memcpy(OS_.data() + Offset, Ptr, Size);
}

} // namespace

void RepoObjectWriter::writeSectionData(const MCAssembler &Asm, MCSection &Sec,
                                        const MCAsmLayout &Layout,
                                        ModuleNamesContainer &Names) {
  auto &Section = static_cast<MCSectionRepo &>(Sec);
  pstore::repo::SectionType St = pstore::repo::SectionType::Data;

  auto const kind = Section.getKind();
  if (kind.isBSS()) {
    St = pstore::repo::SectionType::BSS;
  } else if (kind.isCommon()) {
    St = pstore::repo::SectionType::Common;
  } else if (kind.isData()) {
    St = pstore::repo::SectionType::Data;
  } else if (kind.isReadOnlyWithRel()) {
    St = pstore::repo::SectionType::RelRo;
  } else if (kind.isText()) {
    St = pstore::repo::SectionType::Text;
  } else if (kind.isMergeable1ByteCString()) {
    St = pstore::repo::SectionType::Mergeable1ByteCString;
  } else if (kind.isMergeable2ByteCString()) {
    St = pstore::repo::SectionType::Mergeable2ByteCString;
  } else if (kind.isMergeable4ByteCString()) {
    St = pstore::repo::SectionType::Mergeable4ByteCString;
  } else if (kind.isMergeableConst4()) {
    St = pstore::repo::SectionType::MergeableConst4;
  } else if (kind.isMergeableConst8()) {
    St = pstore::repo::SectionType::MergeableConst8;
  } else if (kind.isMergeableConst16()) {
    St = pstore::repo::SectionType::MergeableConst16;
  } else if (kind.isMergeableConst32()) {
    St = pstore::repo::SectionType::MergeableConst32;
  } else if (kind.isMergeableConst()) {
    St = pstore::repo::SectionType::MergeableConst;
  } else if (kind.isReadOnly()) {
    St = pstore::repo::SectionType::ReadOnly;
  } else if (kind.isThreadBSS()) {
    St = pstore::repo::SectionType::ThreadBSS;
  } else if (kind.isThreadData()) {
    St = pstore::repo::SectionType::ThreadData;
  } else if (kind.isThreadLocal()) {
    St = pstore::repo::SectionType::ThreadLocal;
  } else if (kind.isMetadata()) {
    St = pstore::repo::SectionType::Metadata;
  } else {
    llvm_unreachable("Unknown section type in writeRepoSectionData");
  }

  auto &SC = Contents[Section.hash()];
  SC.push_back(make_unique<pstore::repo::SectionContent>(St));
  pstore::repo::SectionContent &Content = *SC.back();

  // Add the section content to the fragment.
  svector_ostream<decltype(Content.Data)> VecOS{Content.Data};
  raw_pwrite_stream &OldStream = getStream();
  this->setStream(VecOS);
  Asm.writeSectionData(&Section, Layout);
  this->setStream(OldStream);

  auto const &Relocs = Relocations[&Section];
  Content.Xfixups.reserve(Relocs.size());
  for (auto const &Relocation : Relocations[&Section]) {
    // Insert the target symbol name into the set of known names for this
    // module. By gathering just a single instance of each string used in this
    // TU we reduce the number of insertions into the global name set (which are
    // performed with the transaction lock held).
    auto It = Names
                  .insert(std::make_pair(Relocation.Symbol->getName(),
                                         pstore::address::null()))
                  .first;
    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));

    static_assert(sizeof(NamePtr) <= sizeof(pstore::repo::ExternalFixup::Name),
                  "ExternalFixup::Name is not large enough to hold a pointer");
    assert(Relocation.Type <= std::numeric_limits<decltype(
                                  pstore::repo::ExternalFixup::Type)>::max());

    // Attach a suitable external fixup to this section.
    Content.Xfixups.push_back(
        pstore::repo::ExternalFixup{{NamePtr},
                                    static_cast<std::uint8_t>(Relocation.Type),
                                    Relocation.Offset,
                                    Relocation.Addend});
  }
}

void RepoObjectWriter::writeTicketNodes(const MCAssembler &Asm,
                                        ModuleNamesContainer &Names) {
  // Record the TicketMember for this RepoSection.
  auto &TC = TicketContents[Header.uuid];
  for (const TicketNode *Ticket : Asm.getContext().getTickets()) {
    auto It = Names
                  .insert(std::make_pair(Ticket->getNameAsString(),
                                         pstore::address::null()))
                  .first;
    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));
    TC.push_back(pstore::repo::ticket_member{
        pstore::index::uint128{Ticket->getDigest().high(),
                               Ticket->getDigest().low()},
        {NamePtr},
        static_cast<uint8_t>(Ticket->getLinkage()),
        Ticket->isComdat()});
  }
}

namespace {

pstore::database &getDatabase() {
  static std::unique_ptr<pstore::database> Repository;
  if (!Repository) {
    Repository.reset(new pstore::database("./clang.db", true /*writable*/));
  }
  return *Repository;
}

std::pair<pstore::database &, TransactionType &> getTransaction() {
  pstore::database &Repository = getDatabase();
  static auto Transaction = pstore::begin(Repository);
  return {Repository, Transaction};
}

raw_ostream &operator<<(raw_ostream &OS, pstore::index::uint128 const &V) {
  auto digitToHex = [](unsigned v) {
    assert(v < 0x10);
    return static_cast<char>(v + ((v < 10) ? '0' : 'a' - 10));
  };

  std::uint64_t const High = V.high();
  for (int Shift = 64 - 4; Shift >= 0; Shift -= 4) {
    OS << digitToHex((High >> Shift) & 0x0F);
  }

  std::uint64_t const Low = V.low();
  for (int Shift = 64 - 4; Shift >= 0; Shift -= 4) {
    OS << digitToHex((Low >> Shift) & 0x0F);
  }
  return OS;
}

} // namespace

void RepoObjectWriter::writeObject(MCAssembler &Asm,
                                   const MCAsmLayout &Layout) {
  // Write out the ticket file ...
  writeHeader(Asm);
  ModuleNamesContainer Names{100, StringHash};

  raw_fd_ostream &TempStream = static_cast<raw_fd_ostream &>(getStream());

  // Try to get the path from the file descriptor
  SmallString<64> ResultPath;
  std::error_code ErrorCode =
      sys::fs::getPathFromOpenFD(TempStream.get_fd(), ResultPath);
  if (ErrorCode) {
    report_fatal_error(
        "TicketNode: Invalid output file path: " + ErrorCode.message() + ".");
  }
  OutputFile = ResultPath.str();
  dbgs() << "path: " << OutputFile << "\n";

  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Asm, Section, Layout, Names);
  }

  writeTicketNodes(Asm, Names);

  Names.insert(std::make_pair(OutputFile, pstore::address::null()));

  std::pair<pstore::database &, TransactionType &> DbTransact =
      getTransaction();
  auto &Db = DbTransact.first;
  auto &Transaction = DbTransact.second;

  pstore::index::name_index *const NamesIndex = Db.get_name_index();
  assert(NamesIndex);

  // Insert the names from this module into the global name set.
  for (ModuleNamesContainer::value_type &NameAddress : Names) {
    dbgs() << "insert name: " << NameAddress.first << '\n';
    pstore::index::name_index::iterator It =
        NamesIndex->insert(Transaction, NameAddress.first).first;
    NameAddress.second = It.get_address();
  }

  pstore::index::digest_index *const DigestsIndex = Db.get_digest_index();
  assert(DigestsIndex);

  for (auto &Content : Contents) {
    auto const Key =
        pstore::index::uint128{Content.first.high(), Content.first.low()};
    if (DigestsIndex->find(Key) != DigestsIndex->end()) {
      dbgs() << "fragment " << Key << " exists. skipping\n";
    } else {
      auto Begin = pstore::repo::details::makeSectionContentIterator(
          Content.second.begin());
      auto End = pstore::repo::details::makeSectionContentIterator(
          Content.second.end());

      // The name field of each of the external fixups is pointing into the
      // 'Names' map. Here we turn that into the pstore address of the string.
      std::for_each(Begin, End, [](pstore::repo::SectionContent &Section) {
        for (auto &XFixup : Section.Xfixups) {
          auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
              XFixup.Name.absolute());
          XFixup.Name = MNC->second;
        }
      });

      dbgs() << "fragment " << Key << " adding. size="
             << pstore::repo::Fragment::sizeBytes(Begin, End) << '\n';

      pstore::record FragmentRecord =
          pstore::repo::Fragment::alloc(Transaction, Begin, End);
      auto Kvp = std::make_pair(Key, FragmentRecord);
      DigestsIndex->insert(Transaction, Kvp);
    }
  }

  pstore::index::ticket_index *const TicketIndex = Db.get_ticket_index();
  assert(TicketIndex);

  // Find the store addres of output file path.
  auto It = Names.find(OutputFile);
  assert(It != Names.end() && "Output file can't be found!");
  auto OutputPathAddr = It->second;

  for (auto &TicketContent : TicketContents) {

    dbgs() << "Ticket uuid " << TicketContent.first.str() << " adding. \n";

    // The name field of each of ticket_member is pointing into the 'Names' map.
    // Here we turn that into the pstore address of the string.
    for (auto &TicketMember : TicketContent.second) {
      auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
          TicketMember.name.absolute());
      TicketMember.name = MNC->second;
      dbgs() << "ticket name " << TicketMember.name.absolute() << " digest "
             << TicketMember.digest << " adding." << '\n';
    }

    // Store the Ticket into store.
    pstore::record TicketRecord = pstore::repo::ticket::alloc(
        Transaction, OutputPathAddr, TicketContent.second);
    auto Kvp = std::make_pair(TicketContent.first, TicketRecord);
    TicketIndex->insert(Transaction, Kvp);
  }

  Transaction.commit();
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
