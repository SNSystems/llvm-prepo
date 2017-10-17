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
#include "llvm/IR/RepoGlobals.h"
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
#include "pstore_mcrepo/fragment.hpp"
#include "pstore_mcrepo/ticket.hpp"

using namespace llvm;

#undef DEBUG_TYPE
#define DEBUG_TYPE "repo-object"

namespace {
typedef DenseMap<const MCSectionRepo *, uint32_t> SectionIndexMapTy;


auto StringHash = [](StringRef s) { return HashString(s); };

class RepoObjectWriter : public MCObjectWriter {

  /// The target specific repository writer instance.
  std::unique_ptr<MCRepoObjectTargetWriter> TargetObjectWriter;

  DenseMap<const MCSymbolRepo *, const MCSymbolRepo *> Renames;

  DenseMap<const MCSectionRepo *, std::vector<RepoRelocationEntry>> Relocations;

  // Note that I don't use StringMap because we take pointers into this
  // structure that must survive insertion.
  using ModuleNamesContainer =
      std::unordered_map<StringRef, pstore::address, decltype(StringHash)>;

  std::map<Digest::DigestType,
           SmallVector<std::unique_ptr<pstore::repo::section_content>, 4>>
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

  /// @}

  // TargetObjectWriter wrappers.
  bool hasRelocationAddend() const { return true; }
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const {
    return TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
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

  void writeTicketFile(const MCAssembler &Asm);

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

  void writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
};
} // end anonymous namespace

RepoObjectWriter::~RepoObjectWriter() {}

void RepoObjectWriter::writeTicketFile(const MCAssembler &Asm) {
  writeBytes(Header.RepoMagic);
  writeBytes(
      StringRef(reinterpret_cast<const char *>(Header.uuid.array().data()),
                Header.uuid.elements));
}

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

  uint64_t Addend = 0;
  if (hasRelocationAddend()) {
    Addend = C;
    C = 0;
  }

  FixedValue = C;

  const auto *RenamedSymA = SymA;
  if (SymA) {
    if (const MCSymbolRepo *R = Renames.lookup(SymA)) {
      RenamedSymA = R;
    }

    RenamedSymA->setUsedInReloc();
  }
  Relocations[&FixupSection].emplace_back(FixupOffset, RenamedSymA, Type,
                                          Addend, SymA, OriginalC);
}

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
  // A "dummy" section is created to provide a default for the assembler but we don't
  // write it to the repository.
  if (Section.isDummy ()) {
      // TODO: warn if the dummy section is not empty.
      return;
  }

  auto St = pstore::repo::section_type::Data;

  auto const kind = Section.getKind();
  if (kind.isBSS()) {
    St = pstore::repo::section_type::BSS;
  } else if (kind.isCommon()) {
    St = pstore::repo::section_type::Common;
  } else if (kind.isData()) {
    St = pstore::repo::section_type::Data;
  } else if (kind.isReadOnlyWithRel()) {
    St = pstore::repo::section_type::RelRo;
  } else if (kind.isText()) {
    St = pstore::repo::section_type::Text;
  } else if (kind.isMergeable1ByteCString()) {
    St = pstore::repo::section_type::Mergeable1ByteCString;
  } else if (kind.isMergeable2ByteCString()) {
    St = pstore::repo::section_type::Mergeable2ByteCString;
  } else if (kind.isMergeable4ByteCString()) {
    St = pstore::repo::section_type::Mergeable4ByteCString;
  } else if (kind.isMergeableConst4()) {
    St = pstore::repo::section_type::MergeableConst4;
  } else if (kind.isMergeableConst8()) {
    St = pstore::repo::section_type::MergeableConst8;
  } else if (kind.isMergeableConst16()) {
    St = pstore::repo::section_type::MergeableConst16;
  } else if (kind.isMergeableConst32()) {
    St = pstore::repo::section_type::MergeableConst32;
  } else if (kind.isMergeableConst()) {
    St = pstore::repo::section_type::MergeableConst;
  } else if (kind.isReadOnly()) {
    St = pstore::repo::section_type::ReadOnly;
  } else if (kind.isThreadBSS()) {
    St = pstore::repo::section_type::ThreadBSS;
  } else if (kind.isThreadData()) {
    St = pstore::repo::section_type::ThreadData;
  } else if (kind.isThreadLocal()) {
    St = pstore::repo::section_type::ThreadLocal;
  } else if (kind.isMetadata()) {
    St = pstore::repo::section_type::Metadata;
  } else {
    llvm_unreachable("Unknown section type in writeRepoSectionData");
  }

  auto &SC = Contents[Section.hash()];
  SC.push_back(make_unique<pstore::repo::section_content>(St));
  pstore::repo::section_content &Content = *SC.back();

  // Add the section content to the fragment.
  svector_ostream<decltype(Content.data)> VecOS{Content.data};
  raw_pwrite_stream &OldStream = getStream();
  this->setStream(VecOS);
  Asm.writeSectionData(&Section, Layout);
  this->setStream(OldStream);

  auto const &Relocs = Relocations[&Section];
  Content.xfixups.reserve(Relocs.size());
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

    static_assert(sizeof(NamePtr) <= sizeof(pstore::repo::external_fixup::name),
                  "ExternalFixup::Name is not large enough to hold a pointer");
    assert(Relocation.Type <= std::numeric_limits<decltype(
                                  pstore::repo::external_fixup::type)>::max());

    // Attach a suitable external fixup to this section.
    Content.xfixups.push_back(
        pstore::repo::external_fixup{{NamePtr},
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

using TransactionType = pstore::transaction<pstore::transaction_lock>;

/// Returns an active transaction on the pstore database, creating it if
/// not already open.
std::pair<pstore::database &, TransactionType &> getRepoTransaction() {
  pstore::database &Repository = llvm::getRepoDatabase();
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
  writeTicketFile(Asm);

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
  DEBUG(dbgs() << "path: " << OutputFile << "\n");

  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Asm, Section, Layout, Names);
  }

  writeTicketNodes(Asm, Names);

  Names.insert(std::make_pair(OutputFile, pstore::address::null()));

  std::pair<pstore::database &, TransactionType &> DbTransact =
      getRepoTransaction();
  auto &Db = DbTransact.first;
  auto &Transaction = DbTransact.second;

  pstore::index::name_index *const NamesIndex = Db.get_name_index();
  assert(NamesIndex);

  // Insert the names from this module into the global name set.
  for (ModuleNamesContainer::value_type &NameAddress : Names) {
    DEBUG(dbgs() << "insert name: " << NameAddress.first << '\n');
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
      DEBUG(dbgs() << "fragment " << Key << " exists. skipping\n");
    } else {
      auto Begin = pstore::repo::details::make_section_content_iterator(
          Content.second.begin());
      auto End = pstore::repo::details::make_section_content_iterator(
          Content.second.end());

      // The name field of each of the external fixups is pointing into the
      // 'Names' map. Here we turn that into the pstore address of the string.
      std::for_each(Begin, End, [](pstore::repo::section_content &Section) {
        for (auto &XFixup : Section.xfixups) {
          auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
              XFixup.name.absolute());
          XFixup.name = MNC->second;
        }
      });

      DEBUG(dbgs() << "fragment " << Key << " adding. size="
                   << pstore::repo::fragment::size_bytes(Begin, End) << '\n');

      pstore::record FragmentRecord =
          pstore::repo::fragment::alloc(Transaction, Begin, End);
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

    DEBUG(dbgs() << "Ticket uuid " << TicketContent.first.str()
                 << " adding. \n");

    // The name field of each of ticket_member is pointing into the 'Names' map.
    // Here we turn that into the pstore address of the string.
    for (auto &TicketMember : TicketContent.second) {
      auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
          TicketMember.name.absolute());
      TicketMember.name = MNC->second;
      DEBUG(dbgs() << "ticket name " << TicketMember.name.absolute()
                   << " digest " << TicketMember.digest << " adding." << '\n');
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
  }
  return MCObjectWriter::isSymbolRefDifferenceFullyResolvedImpl(Asm, SymA, FB,
                                                                InSet, IsPCRel);
}

MCObjectWriter *llvm::createRepoObjectWriter(MCRepoObjectTargetWriter *MOTW,
                                             raw_pwrite_stream &OS,
                                             bool IsLittleEndian) {
  return new RepoObjectWriter(MOTW, OS, IsLittleEndian);
}
