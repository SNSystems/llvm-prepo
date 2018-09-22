//===- repo2obj - Convert a Repository ticket to an ELF object file ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Object/Binary.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/mcrepo/ticket.hpp"
#include "pstore/support/array_elements.hpp"

#include "R2OELFOutputSection.h"
#include "WriteHelpers.h"

#include <cstdlib>
#include <functional>
#include <list>
#include <tuple>
#include <unordered_map>

#ifdef _WIN32
#include <io.h>
#endif

using namespace llvm;
using namespace llvm::object;

namespace {

cl::opt<std::string> RepoPath("repo", cl::Optional,
                              cl::desc("Program repository path"),
                              cl::init("./clang.db"));
cl::opt<std::string> TicketPath(cl::Positional, cl::desc("<ticket path>"));
cl::opt<std::string> OutputFilename("o", cl::desc("Output filename"),
                                    cl::value_desc("filename"),
                                    cl::init("./a.out"));

} // end anonymous namespace

LLVM_ATTRIBUTE_NORETURN static void error(Twine Message) {
  errs() << Message << "\n";
  exit(EXIT_FAILURE);
}

namespace {

class SpecialNames {
public:
  void initialize(pstore::database &Db, GeneratedNames &Names);

  pstore::typed_address<pstore::indirect_string> CtorName =
      pstore::typed_address<pstore::indirect_string>::null();
  pstore::typed_address<pstore::indirect_string> DtorName =
      pstore::typed_address<pstore::indirect_string>::null();

private:
  static pstore::typed_address<pstore::indirect_string>
  findString(pstore::index::name_index const &NameIndex,
             pstore::indirect_string const &Str);
};

// initialize
// ~~~~~~~~~~
void SpecialNames::initialize(pstore::database &Db, GeneratedNames &Names) {
  std::shared_ptr<pstore::index::name_index const> const NameIndex =
      pstore::index::get_index<pstore::trailer::indices::name>(Db);
  if (!NameIndex) {
    errs() << "Warning: name index was not found.\n";
  } else {
    // Get the address of the global tors names from the names set. If the
    // string is missing, use null since we know that can't appear as a ticket's
    // name.
    CtorName = findString(*NameIndex, Names.add("llvm.global_ctors"));
    DtorName = findString(*NameIndex, Names.add("llvm.global_dtors"));
  }
}

// findString
// ~~~~~~~~~~
pstore::typed_address<pstore::indirect_string>
SpecialNames::findString(pstore::index::name_index const &NameIndex,
                         pstore::indirect_string const &Str) {

  auto Pos = NameIndex.find(Str);
  return (Pos != NameIndex.end())
             ? pstore::typed_address<pstore::indirect_string>(Pos.get_address())
             : pstore::typed_address<pstore::indirect_string>::null();
}

} // end anonymous namespace

template <class ELFT> struct ELFState {
  using Elf_Word = typename object::ELFFile<ELFT>::Elf_Word;
  using Elf_Ehdr = typename object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Phdr = typename object::ELFFile<ELFT>::Elf_Phdr;
  using Elf_Shdr = typename object::ELFFile<ELFT>::Elf_Shdr;
  using Elf_Sym = typename object::ELFFile<ELFT>::Elf_Sym;
  using Elf_Rel = typename object::ELFFile<ELFT>::Elf_Rel;
  using Elf_Rela = typename object::ELFFile<ELFT>::Elf_Rela;

  using SectionHeaderTable = std::vector<Elf_Shdr>;
  SectionHeaderTable SectionHeaders;
  std::map<SectionId, OutputSection<ELFT>> Sections;
  std::map<pstore::typed_address<pstore::indirect_string>, GroupInfo<ELFT>>
      Groups;

  GeneratedNames Generated;
  StringTable Strings;
  SymbolTable<ELFT> Symbols;
  SpecialNames Magics;

  explicit ELFState(pstore::database &Db)
      : Generated{Db}, Strings{Generated}, Symbols{Strings} {}
  void initialize(pstore::database &Db) { Magics.initialize(Db, Generated); }

  void initELFHeader(Elf_Ehdr &Header);
  void initStandardSections();
  uint64_t writeSectionHeaders(raw_ostream &OS);

  void buildGroupSection(pstore::database const &Db, GroupInfo<ELFT> &GI);

  /// Writes the group section data that was recorded by earlier calls to
  /// buildGroupSection(). The group section headers are updated to record the
  /// location and and size of this data.
  void writeGroupSections(raw_ostream &OS);
};

template <class ELFT> void ELFState<ELFT>::initELFHeader(Elf_Ehdr &Header) {
  using namespace llvm::ELF;
  Header.e_ident[EI_MAG0] = 0x7f;
  Header.e_ident[EI_MAG1] = 'E';
  Header.e_ident[EI_MAG2] = 'L';
  Header.e_ident[EI_MAG3] = 'F';
  Header.e_ident[EI_CLASS] = ELFT::Is64Bits ? ELFCLASS64 : ELFCLASS32;
  Header.e_ident[EI_DATA] =
      (ELFT::TargetEndianness == support::little) ? ELFDATA2LSB : ELFDATA2MSB;
  Header.e_ident[EI_VERSION] = EV_CURRENT;
  Header.e_ident[EI_OSABI] = ELFOSABI_NONE;
  Header.e_ident[EI_ABIVERSION] = 0;
  Header.e_type = ET_REL;
  Header.e_machine =
      EM_X86_64; // FIXME: where do we represent the machine type?
  Header.e_version = EV_CURRENT;
  Header.e_entry = 0;
  Header.e_phoff = 0;
  Header.e_shoff = 0; // patched up later
  Header.e_flags = 0;
  Header.e_ehsize = sizeof(Elf_Ehdr);
  Header.e_phentsize = sizeof(Elf_Phdr);
  Header.e_phnum = 0;
  Header.e_shentsize = sizeof(Elf_Shdr);
  Header.e_shnum = 0; // patched up later.
  Header.e_shstrndx = SectionIndices::StringTab;
}

template <typename ELFT> void ELFState<ELFT>::initStandardSections() {
  // null section
  Elf_Shdr SH;
  zero(SH);
  assert(SectionHeaders.size() == SectionIndices::Null);
  SectionHeaders.push_back(SH);

  // string table
  zero(SH);
  SH.sh_name = Strings.insert(Generated.add(".strtab"));
  SH.sh_type = ELF::SHT_STRTAB;
  assert(SectionHeaders.size() == SectionIndices::StringTab);
  SectionHeaders.push_back(SH);

  // Symbol table
  zero(SH);
  SH.sh_name = Strings.insert(Generated.add(".symtab"));
  SH.sh_type = ELF::SHT_SYMTAB;
  SH.sh_link = SectionIndices::StringTab;
  SH.sh_entsize = sizeof(ELFState<ELFT>::Elf_Sym);
  SH.sh_addralign = alignof(ELFState<ELFT>::Elf_Sym);
  assert(SectionHeaders.size() == SectionIndices::SymTab);
  SectionHeaders.push_back(SH);
}

template <typename ELFT>
uint64_t ELFState<ELFT>::writeSectionHeaders(raw_ostream &OS) {
  writeAlignmentPadding<Elf_Shdr>(OS);
  auto const Offset = OS.tell();
  OS.write(reinterpret_cast<char const *>(SectionHeaders.data()),
           SectionHeaders.size() * sizeof(Elf_Shdr));
  return Offset;
}

// The ELF spec. requires the groups sections to appear before the sections that
// they contain in the section header table. For this reason, we have to create
// them in two passes: the first creates the headers; later, once we know the
// collections of the section indices that they'll contain, we can write the
// group section bodies.
//
// A further wrinkle is that the entry in the section header table contains the
// index of the group's "signature" symbol. We therefore must have already
// generated and sorted the symbol table to assign indices.
template <typename ELFT>
void ELFState<ELFT>::buildGroupSection(pstore::database const &Db,
                                       GroupInfo<ELFT> &GI) {
  // If we haven't yet recorded a section index for this group, then build one
  // now.
  if (GI.SectionIndex == 0U) {
    auto SignatureSymbol = Symbols.findSymbol(
        pstore::indirect_string::read(Db, GI.IdentifyingSymbol));
    assert(SignatureSymbol != nullptr &&
           SignatureSymbol->Index != llvm::ELF::STN_UNDEF);
    static auto const GroupString = Generated.add(".group");
    ELFState<ELFT>::Elf_Shdr SH;
    zero(SH);
    SH.sh_name = Strings.insert(GroupString);
    SH.sh_type = ELF::SHT_GROUP;
    SH.sh_link = SectionIndices::SymTab;
    SH.sh_info = SignatureSymbol->Index; // The group's signature symbol entry.
    SH.sh_entsize = sizeof(ELF::Elf32_Word);
    SH.sh_addralign = alignof(ELF::Elf32_Word);

    GI.SectionIndex = SectionHeaders.size();
    SectionHeaders.push_back(SH);
  }
}

template <typename ELFT>
void ELFState<ELFT>::writeGroupSections(raw_ostream &OS) {
  for (auto const &G : Groups) {
    writeAlignmentPadding<ELF::Elf32_Word>(OS);
    auto const StartPos = OS.tell();
    auto NumWords = 1U;
    writeRaw(OS, ELF::Elf32_Word{ELF::GRP_COMDAT});

    for (OutputSection<ELFT> const *GroupMember : G.second.Members) {
      auto SectionIndex = GroupMember->getIndex();
      writeRaw(OS, ELF::Elf32_Word{SectionIndex});
      ++NumWords;

      if (GroupMember->numRelocations() > 0) {
        writeRaw(OS, ELF::Elf32_Word{SectionIndex + 1});
        ++NumWords;
      }
    }

    auto const SectionSize = NumWords * sizeof(ELF::Elf32_Word);
    assert(OS.tell() - StartPos == SectionSize);

    assert(G.second.SectionIndex < SectionHeaders.size());
    Elf_Shdr &SH = SectionHeaders[G.second.SectionIndex];
    assert(SH.sh_type == ELF::SHT_GROUP);
    SH.sh_offset = StartPos;
    SH.sh_size = SectionSize;
  }
}

static ELFSectionType
getELFSectionType(pstore::repo::section_kind Kind,
                  pstore::typed_address<pstore::indirect_string> Name,
                  SpecialNames const &Magics) {
  if (Name == Magics.CtorName) {
    return ELFSectionType::init_array;
  } else if (Name == Magics.DtorName) {
    return ELFSectionType::fini_array;
  }

#define REPO_TO_ELF_SECTION(a)  case (pstore::repo::section_kind::a): return (ELFSectionType::a);

  switch (Kind) {
    REPO_TO_ELF_SECTION(text)
    REPO_TO_ELF_SECTION(bss)
    REPO_TO_ELF_SECTION(data)
    REPO_TO_ELF_SECTION(rel_ro)
    REPO_TO_ELF_SECTION(mergeable_1_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_2_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_4_byte_c_string)
    REPO_TO_ELF_SECTION(mergeable_const_4)
    REPO_TO_ELF_SECTION(mergeable_const_8)
    REPO_TO_ELF_SECTION(mergeable_const_16)
    REPO_TO_ELF_SECTION(mergeable_const_32)
    REPO_TO_ELF_SECTION(read_only)
    REPO_TO_ELF_SECTION(thread_bss)
    REPO_TO_ELF_SECTION(thread_data)
  case pstore::repo::section_kind::dependent:
  case pstore::repo::section_kind::last:
    break;
  }
#undef REPO_TO_ELF_SECTION
  llvm_unreachable("getELFSectionType: unknown repository section kind.");
}

static std::string getRepoPath() {
  if (RepoPath.getNumOccurrences() == 0) {
    // TODO: remove this envrionment variable once the matching behavior is
    // removed from the compiler.
    if (auto File = getenv("REPOFILE")) {
      return {File};
    }
  }
  return RepoPath;
}

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
  return OS << Digest.to_hex_string();
}


int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv);

  std::error_code EC;
  std::unique_ptr<ToolOutputFile> Out(
      new ToolOutputFile(OutputFilename, EC, sys::fs::F_None));
  if (EC) {
    error("repo2obj: Error opening '" + OutputFilename + "': " + EC.message());
  }

  ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  pstore::index::digest const &Digest = DigestOrError.get();
  LLVM_DEBUG(dbgs() << "'" << TicketPath << "' : " << Digest << '\n');

  pstore::database Db(getRepoPath(), pstore::database::access_mode::read_only);
  std::shared_ptr<pstore::index::ticket_index const> const TicketIndex =
      pstore::index::get_index<pstore::trailer::indices::ticket>(Db);
  if (!TicketIndex) {
    errs() << "Error: ticket index was not found.\n";
    return EXIT_FAILURE;
  }
  std::shared_ptr<pstore::index::digest_index const> const FragmentIndex =
      pstore::index::get_index<pstore::trailer::indices::digest>(Db);
  if (!FragmentIndex) {
    errs() << "Error: fragment index was not found.\n";
    return EXIT_FAILURE;
  }

  auto TicketPos = TicketIndex->find(Digest);
  if (TicketPos == TicketIndex->end()) {
    errs() << "Error: ticket " << Digest << " was not found.\n";
    return EXIT_FAILURE;
  }

  using ELFT = ELF64LE;
  ELFState<ELF64LE> State(Db);
  State.initialize(Db);

  {
    std::vector<OutputSection<ELFT>::SectionInfo> OutputSections;
    OutputSections.resize(::pstore::repo::fragment::member_array::max_size());

    auto Ticket = pstore::repo::ticket::load(Db, TicketPos->second);
    for (auto const &TM : *Ticket) {
      assert(TM.name != pstore::typed_address<pstore::indirect_string>::null());
      LLVM_DEBUG(dbgs() << "Processing: "
                        << pstore::indirect_string::read(Db, TM.name) << '\n');

      auto const FragmentPos = FragmentIndex->find(TM.digest);
      if (FragmentPos == FragmentIndex->end()) {
        errs() << "Error: fragment " << TM.digest << " was not found.\n";
        return EXIT_FAILURE;
      }

      std::fill(std::begin(OutputSections), std::end(OutputSections),
                OutputSection<ELFT>::SectionInfo{});

      auto const IsLinkOnce =
          TM.linkage == pstore::repo::linkage_type::linkonce;
      // TODO: enable the name discriminator if "function/data sections mode" is
      // enabled.
      auto const Discriminator =
          IsLinkOnce ? TM.name
                     : pstore::typed_address<pstore::indirect_string>::null();
      auto const Fragment =
          pstore::repo::fragment::load(Db, FragmentPos->second);

      if (TM.linkage == pstore::repo::linkage_type::common) {
        auto const Name = pstore::indirect_string::read(Db, TM.name);
        assert(Name.is_in_store());

        if (!Fragment->has_section(pstore::repo::section_kind::bss)
            || std::count_if (Fragment->begin (), Fragment->end (), pstore::repo::is_target_section) != 1) {

          pstore::shared_sstring_view Owner;
          error("Fragment for common symbol \"" +
                Name.as_string_view(&Owner).to_string() +
                "\" did not contain a sole BSS section");
        }

        pstore::repo::generic_section const &S =
            Fragment->at<pstore::repo::section_kind::bss>();
        State.Symbols.insertSymbol(Name, nullptr /*no output section*/,
                                   0 /*offset*/, S.data().size(), TM.linkage);
        continue;
      }
      // Go through the sections that this fragment contains creating the
      // corresponding ELF section(s) as necessary.
      auto const SectionRange = make_filter_range(
          make_range(std::begin(*Fragment), std::end(*Fragment)),
          pstore::repo::is_target_section);

      for (pstore::repo::section_kind Section : SectionRange) {
        assert (pstore::repo::is_target_section (Section));

        // The section type and "discriminator" together identify the ELF output
        // section to which this fragment's section data will be appended.
        auto const Id = std::make_tuple(
            getELFSectionType(Section, TM.name, State.Magics),
            Discriminator);

        decltype(State.Sections)::iterator Pos;
        bool DidInsert;
        std::tie(Pos, DidInsert) =
            State.Sections.emplace(Id, OutputSection<ELFT>(Db, Id));

        OutputSection<ELFT> *const OSection = &Pos->second;

        // If this is the first time that we've wanted to append to the ELF
        // section described by 'Id' and the ticket-members has linkonce
        // linkage, then we need to make the section a member of a group
        // section.
        if (DidInsert && IsLinkOnce) {
          decltype(State.Groups)::iterator GroupPos =
              State.Groups.find(TM.name);
          if (GroupPos == State.Groups.end()) {
            bool _;
            std::tie(GroupPos, _) =
                State.Groups.emplace(TM.name, GroupInfo<ELFT>(TM.name));
          }

          GroupPos->second.Members.push_back(OSection);

          // Tell the output section about the group of which it's a member.
          OSection->attachToGroup(&GroupPos->second);
        }

        // Record the location that the later call to section()->append() will
        // assign to this data. We need to account for any alignment padding
        // that that function may place before the data itself (hence the call
        // to alignedContributionSize()).
        OutputSections[static_cast<unsigned>(Section)] =
            OutputSection<ELFT>::SectionInfo(
                OSection,
                OSection->alignedContributionSize(
                    pstore::repo::section_align(*Fragment, Section)));
      }

      // This can't currently be folded into the first loop because it needs the
      // OutputSections array to be built first.
      for (pstore::repo::section_kind Section : SectionRange) {
        OutputSections[static_cast<unsigned>(Section)].section()->append(
            TM, Fragment, Section, State.Symbols, State.Generated,
            OutputSections);
      }
    }
  }

  LLVM_DEBUG(dbgs() << "There are " << State.Groups.size() << " groups\n");

  std::vector<SymbolTable<ELFT>::Value *> OrderedSymbols = State.Symbols.sort();

  decltype(State)::Elf_Ehdr Header;
  State.initELFHeader(Header);
  State.initStandardSections();

  auto &OS = Out->os();
  writeRaw(OS, Header);

  for (auto &S : State.Sections) {
    OutputSection<ELFT> &Section = S.second;
    if (GroupInfo<ELFT> *const Group = Section.group()) {
      State.buildGroupSection(Db, *Group);
    }
    Section.setIndex(State.SectionHeaders.size());
    Section.write(OS, State.Strings, State.Generated,
                  std::back_inserter(State.SectionHeaders));
  }

  State.writeGroupSections(OS);

  // Write the string table (and patch its section header)
  {
    auto &S = State.SectionHeaders[SectionIndices::StringTab];
    std::tie(S.sh_offset, S.sh_size) = State.Strings.write(OS);
  }
  // Now do the same for the symbol table.
  {
    auto &S = State.SectionHeaders[SectionIndices::SymTab];
    // st_info should be one greater than the symbol table index of the last
    // local symbol (binding STB_LOCAL).
    S.sh_info = SymbolTable<ELFT>::firstNonLocal(OrderedSymbols);
    std::tie(S.sh_offset, S.sh_size) = State.Symbols.write(OS, OrderedSymbols);
  }

  uint64_t SectionHeadersOffset = State.writeSectionHeaders(OS);
  Header.e_shoff = SectionHeadersOffset;
  Header.e_shnum = State.SectionHeaders.size();
  Header.e_shstrndx = SectionIndices::StringTab;

  OS.seek(0);
  writeRaw(OS, Header);

  //  if (Res == 0)
  Out->keep();

  Out->os().flush();

  return EXIT_SUCCESS;
}
