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
#include "llvm/MC/MCRepoTicketFile.h"
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
#include "llvm/Support/Path.h"
#include "llvm/Support/StringSaver.h"
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/core/transaction.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/mcrepo/ticket.hpp"
#include "pstore/support/sstring_view.hpp"

using namespace llvm;

#undef DEBUG_TYPE
#define DEBUG_TYPE "repo-object"

namespace {
typedef DenseMap<const MCSectionRepo *, uint32_t> SectionIndexMapTy;

class RepoObjectWriter : public MCObjectWriter {
private:
  /// The target specific repository writer instance.
  std::unique_ptr<MCRepoObjectTargetWriter> TargetObjectWriter;

  DenseMap<const MCSymbolRepo *, const MCSymbolRepo *> Renames;

  DenseMap<const MCSectionRepo *, std::vector<RepoRelocationEntry>> Relocations;

  // Note that I don't use StringMap because we take pointers into this
  // structure that must survive insertion.
  // TODO: Compare the performance between std::map and std::unordered_map. If
  // the std::unordered_map operation is faster than std::unordered_map, we
  // should use the std::unordered_map and store the ordered module string set
  // into the database later.
  using ModuleNamesContainer =
      std::map<pstore::raw_sstring_view,
               pstore::typed_address<pstore::indirect_string>>;

  using NamesWithPrefixContainer =
      SmallVector<std::unique_ptr<std::string>, 16>;

  // TODO: investigate changing the Key type from ticketmd::DigestType to
  // pstore::index::digest.
  using ContentsType =
      std::map<ticketmd::DigestType,
               SmallVector<std::unique_ptr<pstore::repo::section_content>, 4>>;

  std::vector<pstore::repo::ticket_member> TicketContents;

  BumpPtrAllocator Alloc;
  StringSaver VersionSymSaver{Alloc};

  /// @}

  // TargetObjectWriter wrappers.
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const {
    return TargetObjectWriter->getRelocType(Ctx, Target, Fixup, IsPCRel);
  }

public:
  RepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                   raw_pwrite_stream &OS, bool IsLittleEndian)
      : MCObjectWriter(OS, IsLittleEndian),
        TargetObjectWriter(std::move(MOTW)) {}

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

  void recordRelocation(MCAssembler &Asm, const MCAsmLayout &Layout,
                        const MCFragment *Fragment, const MCFixup &Fixup,
                        MCValue Target, uint64_t &FixedValue) override;

  // Map from a signature symbol to the group section index
  typedef DenseMap<const MCSymbol *, unsigned> RevGroupMapTy;

  void executePostLayoutBinding(MCAssembler &Asm,
                                const MCAsmLayout &Layout) override;

  void writeSectionData(ContentsType &Contents, const MCAssembler &Asm,
                        MCSection &Sec, const MCAsmLayout &Layout,
                        ModuleNamesContainer &Names);

  static pstore::raw_sstring_view
  getSymbolName(const MCAssembler &Asm, const TicketNode &TicketMember,
                const ModuleNamesContainer &Names,
                NamesWithPrefixContainer &Symbols);

  pstore::index::digest buildTicketRecord(const MCAssembler &Asm,
                                          ModuleNamesContainer &Names,
                                          NamesWithPrefixContainer &Symbols,
                                          const ContentsType &Fragments,
                                          StringRef OutputFile);

  static pstore::repo::linkage_type
  toPstoreLinkage(GlobalValue::LinkageTypes L);

  bool isSymbolRefDifferenceFullyResolvedImpl(const MCAssembler &Asm,
                                              const MCSymbol &SymA,
                                              const MCFragment &FB, bool InSet,
                                              bool IsPCRel) const override;

  void writeObject(MCAssembler &Asm, const MCAsmLayout &Layout) override;
};
} // end anonymous namespace

RepoObjectWriter::~RepoObjectWriter() {}

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
                                        uint64_t &FixedValue) {
  MCAsmBackend &Backend = Asm.getBackend();
  bool IsPCRel = Backend.getFixupKindInfo(Fixup.getKind()).Flags &
                 MCFixupKindInfo::FKF_IsPCRel;
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

  //bool ViaWeakRef = false;
  if (SymA && SymA->isVariable()) {
    const MCExpr *Expr = SymA->getVariableValue();
    if (const auto *Inner = dyn_cast<MCSymbolRefExpr>(Expr)) {
      if (Inner->getKind() == MCSymbolRefExpr::VK_WEAKREF) {
        SymA = cast<MCSymbolRepo>(&Inner->getSymbol());
        //ViaWeakRef = true; TODO: we're not supporting weak references at the moment.
      }
    }
  }

  unsigned Type = getRelocType(Ctx, Target, Fixup, IsPCRel);
  uint64_t OriginalC = C;
  bool RelocateWithSymbol = false;
  if (!RelocateWithSymbol && SymA && !SymA->isUndefined())
    C += Layout.getSymbolOffset(*SymA);

  uint64_t Addend = C;
  FixedValue = 0;

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

pstore::raw_sstring_view stringRefAsView(StringRef S) {
  return {S.data(), S.size()};
}

StringRef stringViewAsRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}

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

static pstore::repo::section_type SectionKindToRepoType(SectionKind K) {
  if (K.isText()) {
    return pstore::repo::section_type::text;
  }

  // TODO: add sections types for BSSLocal and BSSExtern?
  if (K.isBSS() || K.isCommon()) {
    return pstore::repo::section_type::bss;
  }
  if (K.isData()) {
    return pstore::repo::section_type::data;
  }
  if (K.isMergeableConst4()) {
    return pstore::repo::section_type::mergeable_const_4;
  }
  if (K.isMergeableConst8()) {
    return pstore::repo::section_type::mergeable_const_8;
  }
  if (K.isMergeableConst16()) {
    return pstore::repo::section_type::mergeable_const_16;
  }
  if (K.isMergeableConst32()) {
    return pstore::repo::section_type::mergeable_const_32;
  }
  assert(!K.isMergeableConst() &&
         "isMergeableConst should be covered by the four previous checks");

  if (K.isMergeable1ByteCString()) {
    return pstore::repo::section_type::mergeable_1_byte_c_string;
  }
  if (K.isMergeable2ByteCString()) {
    return pstore::repo::section_type::mergeable_2_byte_c_string;
  }
  if (K.isMergeable4ByteCString()) {
    return pstore::repo::section_type::mergeable_4_byte_c_string;
  }
  assert(!K.isMergeableCString() &&
         "isMergeableCString should be covered by the three previous checks");

  if (K.isReadOnly()) {
    return pstore::repo::section_type::read_only;
  }
  if (K.isReadOnlyWithRel()) {
    return pstore::repo::section_type::rel_ro;
  }
  if (K.isThreadBSS()) {
    return pstore::repo::section_type::thread_bss;
  }
  if (K.isThreadData()) {
    return pstore::repo::section_type::thread_data;
  }
  assert(!K.isThreadLocal() &&
         "isThreadLocation should be covered by the two previous checks");

  llvm_unreachable("Unsupported section type in getRepoSection");
}

void RepoObjectWriter::writeSectionData(ContentsType &Fragments,
                                        const MCAssembler &Asm, MCSection &Sec,
                                        const MCAsmLayout &Layout,
                                        ModuleNamesContainer &Names) {
  auto &Section = static_cast<MCSectionRepo &>(Sec);

  pstore::repo::section_type const St =
      SectionKindToRepoType(Section.getKind());
  assert(Sec.getAlignment() > 0);
  unsigned const Alignment = Sec.getAlignment();

  // TODO: need a cleaner way to check that the alignment value will fit.
  assert(Alignment <= std::numeric_limits<std::uint8_t>::max());

  auto Content = make_unique<pstore::repo::section_content>(
      St, static_cast<std::uint8_t>(Alignment));

  // Add the section content to the fragment.
  svector_ostream<decltype(Content->data)> VecOS{Content->data};
  raw_pwrite_stream &OldStream = getStream();
  this->setStream(VecOS);
  Asm.writeSectionData(&Section, Layout);
  this->setStream(OldStream);

  auto const &Relocs = Relocations[&Section];
  Content->xfixups.reserve(Relocs.size());
  for (auto const &Relocation : Relocs) {
    using repo_relocation_type = pstore::repo::relocation_type;
    assert (Relocation.Type >= std::numeric_limits <repo_relocation_type>::min ()
            && Relocation.Type <= std::numeric_limits <repo_relocation_type>::max ());

    MCSymbolRepo const *const Symbol = Relocation.Symbol;
    if (Symbol->isInSection(false)) {
      MCSection &S = Symbol->getSection();
      if (MCSectionRepo const *const TargetSection =
              dyn_cast<MCSectionRepo>(&S)) {
        if (TargetSection->hash() == Section.hash()) {
          Content->ifixups.emplace_back(
               SectionKindToRepoType(TargetSection->getKind()),
               static_cast<repo_relocation_type>(Relocation.Type), Relocation.Offset,
               Relocation.Addend);

          continue;
        }
      }
    }

    // Insert the target symbol name into the set of known names for this
    // module. By gathering just a single instance of each string used in this
    // TU we reduce the number of insertions into the global name set (which are
    // performed with the transaction lock held).
    auto It = Names
                  .emplace(stringRefAsView(Relocation.Symbol->getName()),
                           pstore::address::null())
                  .first;
    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));

    static_assert(sizeof(NamePtr) <= sizeof(pstore::repo::external_fixup::name),
                  "ExternalFixup::Name is not large enough to hold a pointer");
    assert(Relocation.Type <= std::numeric_limits<decltype(
                                  pstore::repo::external_fixup::type)>::max());

    // Attach a suitable external fixup to this section.
    Content->xfixups.push_back(pstore::repo::external_fixup{
        pstore::typed_address<pstore::indirect_string>(
            pstore::address{NamePtr}),
        static_cast<repo_relocation_type>(Relocation.Type), Relocation.Offset,
        Relocation.Addend});
  }

  DEBUG(dbgs() << "section type '" << Content->type << "' and alignment "
               << unsigned(Content->align) << '\n');

  // A "dummy" section is created to provide a default for the assembler but we
  // don't write it to the repository.
  if (Section.isDummy()) {
    if (Content->data.size() > 0) {
      llvm_unreachable("The dummy section must have no data payload");
    }
    if (Content->xfixups.size() > 0) {
      llvm_unreachable("The dummy section must have no external fixups");
    }
    if (Content->ifixups.size() > 0) {
      llvm_unreachable("The dummy section must have no internal fixups");
    }

    return;
  }

  Fragments[Section.hash()].push_back(std::move(Content));
}

pstore::repo::linkage_type
RepoObjectWriter::toPstoreLinkage(GlobalValue::LinkageTypes L) {
  switch (L) {
  case GlobalValue::ExternalLinkage:
    return pstore::repo::linkage_type::external;
  case GlobalValue::LinkOnceAnyLinkage:
  case GlobalValue::LinkOnceODRLinkage:
  case GlobalValue::WeakODRLinkage:
    return pstore::repo::linkage_type::linkonce;
  case GlobalValue::PrivateLinkage:
  case GlobalValue::InternalLinkage:
    return pstore::repo::linkage_type::internal;
  case GlobalValue::CommonLinkage:
    return pstore::repo::linkage_type::common;
  case GlobalValue::AppendingLinkage:
    return pstore::repo::linkage_type::append;
  default:
    report_fatal_error("Unsupported linkage type");
  }
}

pstore::raw_sstring_view RepoObjectWriter::getSymbolName(
    const MCAssembler &Asm, const TicketNode &TicketMember,
    const ModuleNamesContainer &Names, NamesWithPrefixContainer &Symbols) {

  if (!GlobalValue::isPrivateLinkage(TicketMember.getLinkage())) {
    StringRef S = TicketMember.getNameAsString();
    return stringRefAsView(S);
  }

  SmallString<256> Buf;
  const StringRef NameRef =
      (Twine(Asm.getContext().getAsmInfo()->getPrivateGlobalPrefix()) +
       Twine(TicketMember.getNameAsString()))
          .toStringRef(Buf);

  auto It = Names.find(stringRefAsView(NameRef));
  if (It != Names.end())
    return It->first;

  Symbols.push_back(llvm::make_unique<std::string>(NameRef.str()));
  return pstore::make_sstring_view(*Symbols.back().get());
}

namespace {

using TransactionType = pstore::transaction<pstore::transaction_lock>;

/// Returns an active transaction on the pstore database, creating it if
/// not already open.
TransactionType &getRepoTransaction() {
  pstore::database &Repository = llvm::getRepoDatabase();
  static auto Transaction = pstore::begin(Repository);
  return Transaction;
}

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &V) {
  return OS << V.to_hex_string();
}

template <typename StringStorage = SmallString<64>>
StringRef streamPath(raw_fd_ostream &Stream, StringStorage &ResultPath) {
  // Try to get the path from the file descriptor
  std::error_code ErrorCode =
      sys::fs::getPathFromOpenFD(Stream.get_fd(), ResultPath);
  if (ErrorCode) {
    report_fatal_error("TicketNode: Invalid output file path: " +
                       ErrorCode.message() + ".");
  }
  llvm::sys::path::remove_filename(ResultPath);
  StringRef OutputFile = ResultPath.str();
  DEBUG(dbgs() << "path: " << OutputFile << "\n");
  return OutputFile;
}

template <typename T> ArrayRef<std::uint8_t> makeByteArrayRef(T const &Value) {
  return {reinterpret_cast<std::uint8_t const *>(&Value), sizeof(Value)};
}

} // namespace

pstore::index::digest RepoObjectWriter::buildTicketRecord(
    const MCAssembler &Asm, ModuleNamesContainer &Names,
    NamesWithPrefixContainer &Symbols, const ContentsType &Fragments,
    StringRef OutputFile) {
  MD5 ticket_hash;

  ticket_hash.update(OutputFile.size());
  ticket_hash.update(OutputFile);

  for (const auto TicketPair : Asm.getContext().getTickets()) {
    const TicketNode *const Ticket = TicketPair.first;
    ticketmd::DigestType const D = Ticket->getDigest();
    // Insert this name into the module-wide string set. This set is later
    // added to the whole-program string set and the ticket name addresses
    // corrected at that time.
    const pstore::raw_sstring_view Name =
        getSymbolName(Asm, *Ticket, Names, Symbols);
    auto It = Names.emplace(Name, pstore::address::null()).first;
    // We're storing pointer to the string address into the ticket.
    auto NamePtr = reinterpret_cast<std::uintptr_t>(&(*It));

    auto DigestVal = pstore::index::digest{D.high(), D.low()};
    auto Linkage = toPstoreLinkage(Ticket->getLinkage());
    // If this TicketNode was created by the backend, it might not be generated
    // when the same input file is built again. Therefore, it doesn't contribute
    // to the ticket hash.
    if (!TicketPair.second) {
      ticket_hash.update(makeByteArrayRef(DigestVal));
      ticket_hash.update(makeByteArrayRef(Linkage));
      ticket_hash.update(Name.size());
      ticket_hash.update(stringViewAsRef(Name));
    }
    // If the global object was removed during LLVM's transform passes, this
    // member is not emitted and doesn't insert to the database, but it does
    // contribute to the hash.
    if (Ticket->getPruned() || Fragments.find(D) != Fragments.end())
      TicketContents.emplace_back(
          DigestVal,
          pstore::typed_address<pstore::indirect_string>(
              pstore::address{NamePtr}),
          Linkage);
  }

  MD5::MD5Result digest;
  ticket_hash.final(digest);
  return {digest.high(), digest.low()};
}

/// Return true if this ticket is already existing in the database ticket index.
static bool isExistingTicket(pstore::database &Db,
                             const pstore::index::digest &TicketDigest) {
  if (auto TicketIndex = pstore::index::get_ticket_index(Db, false)) {
    if (TicketIndex->find(TicketDigest) != TicketIndex->end()) {
      DEBUG(dbgs() << "ticket " << TicketDigest << " exists. skipping\n");
      return true;
    }
  }
  return false;
}

void RepoObjectWriter::writeObject(MCAssembler &Asm,
                                   const MCAsmLayout &Layout) {

  ContentsType Fragments;
  ModuleNamesContainer Names;
  NamesWithPrefixContainer PrefixedNames;

  SmallString<64> ResultPath;
  StringRef OutputFile =
      streamPath(static_cast<raw_fd_ostream &>(this->getStream()), ResultPath);
  Names.emplace(stringRefAsView(OutputFile), pstore::address::null());

  // Convert the Asm sections to repository fragment sections.
  for (MCSection &Sec : Asm) {
    auto &Section = static_cast<MCSectionRepo &>(Sec);
    writeSectionData(Fragments, Asm, Section, Layout, Names);
  }

  pstore::index::digest const TicketDigest =
      buildTicketRecord(Asm, Names, PrefixedNames, Fragments, OutputFile);

  pstore::database &Db = llvm::getRepoDatabase();
  pstore::indirect_string_adder NameAdder(Names.size());

  if (!isExistingTicket(Db, TicketDigest)) {
    TransactionType &Transaction = getRepoTransaction();

    std::shared_ptr<pstore::index::ticket_index> const TicketIndex =
        pstore::index::get_ticket_index(Db);
    assert(TicketIndex);

    std::shared_ptr<pstore::index::name_index> const NamesIndex =
        pstore::index::get_name_index(Db);
    assert(NamesIndex);

    // Insert the names from this module into the global name set. This loop
    // writes the "indirect_string" records which will become pointers to the
    // real string body. This clusters the pointers together nicely which should
    // help to limit the virtual memory consumption of the repo-linker's
    // store-shadow memory.
    for (ModuleNamesContainer::value_type &NameAddress : Names) {
      DEBUG(dbgs() << "insert name: " << stringViewAsRef(NameAddress.first)
                   << '\n');
      pstore::index::name_index::iterator const Pos =
          NameAdder.add(Transaction, NamesIndex, &NameAddress.first).first;
      NameAddress.second =
          pstore::typed_address<pstore::indirect_string>(Pos.get_address());
    }
    // Flush the name bodies.
    NameAdder.flush(Transaction);

    std::shared_ptr<pstore::index::digest_index> const DigestsIndex =
        pstore::index::get_digest_index(Db);
    assert(DigestsIndex);

    for (auto &Fragment : Fragments) {
      auto const Key =
          pstore::index::digest{Fragment.first.high(), Fragment.first.low()};

      // The fragment creation APIs require that the input sections are sorted
      // by section_content::type. This guarantees that for them.
      std::sort(Fragment.second.begin(), Fragment.second.end(),
                [](std::unique_ptr<pstore::repo::section_content> const &a,
                   std::unique_ptr<pstore::repo::section_content> const &b) {
                  return a->type < b->type;
                });
      auto Begin = pstore::repo::details::make_section_content_iterator(
          Fragment.second.begin());
      auto End = pstore::repo::details::make_section_content_iterator(
          Fragment.second.end());

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

      auto const Kvp = std::make_pair(Key, pstore::repo::fragment::alloc(Transaction, Begin, End));
      DigestsIndex->insert(Transaction, Kvp);
    }

    // Find the store address of the output file path.
    auto const OutputFilePos = Names.find(stringRefAsView(OutputFile));
    assert(OutputFilePos != Names.end() && "Output file can't be found!");
    auto OutputPathAddr = OutputFilePos->second;

    // The name field of each of ticket_member is pointing into the 'Names' map.
    // Here we turn that into the pstore address of the string.
    for (pstore::repo::ticket_member &TicketMember : TicketContents) {
      // Check that we have a fragment for this ticket member's digest value.
      // TODO: remove this check once we're completely confident in the back-end
      // implementation.
      if (DigestsIndex->find(TicketMember.digest) == DigestsIndex->end()) {
        report_fatal_error("The digest of missing repository fragment " +
                           TicketMember.digest.to_hex_string() +
                           " was found in a ticket member.");
      }

      auto MNC = reinterpret_cast<ModuleNamesContainer::value_type const *>(
          TicketMember.name.absolute());
      TicketMember.name = MNC->second;
      DEBUG(dbgs() << "ticket name '" << stringViewAsRef(MNC->first)
                   << "' digest '" << TicketMember.digest << "' adding."
                   << '\n');
    }

    // Store the Ticket.
    auto const Kvp = std::make_pair(
        TicketDigest, pstore::repo::ticket::alloc(Transaction, OutputPathAddr,
                                                  TicketContents));
    TicketIndex->insert(Transaction, Kvp);

    Transaction.commit();
  }

  // write the ticket file itself
  llvm::repo::writeTicketFile(this->getStream(), this->isLittleEndian(),
                              TicketDigest);
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

std::unique_ptr<MCObjectWriter>
llvm::createRepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                             raw_pwrite_stream &OS, bool IsLittleEndian) {
  return llvm::make_unique<RepoObjectWriter>(std::move(MOTW), OS,
                                             IsLittleEndian);
}
