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
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/BinaryFormat/Repo.h"
#include "llvm/MC/StringTableBuilder.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/database.hpp"
#include "pstore/hamt_map.hpp"
#include "pstore_mcrepo/fragment.hpp"
#include "pstore_mcrepo/ticket.hpp"

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

    cl::opt<std::string> RepoPath (cl::Optional, cl::desc ("Program pepository path"),
                                   cl::init ("./clang.db"));
    cl::opt<std::string> TicketPath (cl::Positional, cl::desc ("<ticket path>"));
    static cl::opt<std::string> OutputFilename ("o", cl::desc ("Output filename"),
                                                cl::value_desc ("filename"));

} // end anonymous namespace


static ErrorOr<pstore::uuid> getTicketFileUuid (StringRef TicketPath) {
    constexpr auto TicketFileSize = sizeof (std::uint64_t) + pstore::uuid::elements;
    int TicketFD = 0;
    if (std::error_code Err = sys::fs::openFileForRead (TicketPath, TicketFD)) {
        return Err;
    }

    sys::fs::file_status Status;
    if (std::error_code Err = sys::fs::status (TicketFD, Status)) {
        return Err;
    }
    uint64_t FileSize = Status.getSize ();
    if (FileSize != TicketFileSize) {
        // doesn't look like a ticket file.
        assert (0); // FIXME: return a proper error code.
    }

    ErrorOr<std::unique_ptr<MemoryBuffer>> MemberBufferOrErr =
        MemoryBuffer::getOpenFile (TicketFD, TicketPath, FileSize, false);
    if (!MemberBufferOrErr) {
        return MemberBufferOrErr.getError ();
    }

    if (close (TicketFD) != 0) {
        return std::error_code (errno, std::generic_category ());
    }

    StringRef Contents = MemberBufferOrErr.get ()->getBuffer ();
    assert (Contents.size () == TicketFileSize);

    StringRef Signature = Contents.slice (0, 8);
    if (Signature != "RepoUuid") {
        // don't look like a ticket file.
        assert (0); // FIXME: return a proper error code.
    }

    pstore::uuid::container_type UuidBytes;
    StringRef Bytes = Contents.substr (8, pstore::uuid::elements);
    assert (Bytes.size () == pstore::uuid::elements);
    std::copy (std::begin (Bytes), std::end (Bytes), std::begin (UuidBytes));
    return {UuidBytes};
}


template <class ELFT>
struct ELFState {
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
    std::map<pstore::address, std::vector<OutputSection<ELFT> *>> Groups;

    SectionNameStringTable SectionNames;
    StringTable<pstore::address> SymbolNames;
    SymbolTable<ELFT> Symbols;

    explicit ELFState (pstore::database & Db)
            : SymbolNames{StringPolicy<pstore::address> (Db)}
            , Symbols{SymbolNames} {}

    void initELFHeader (Elf_Ehdr & Header);
    void initStandardSections ();
    uint64_t writeSectionHeaders (raw_ostream & OS);

    std::size_t buildGroupSections ();

    /// Writes the group section data that was recorded by an earlier call to buildGroupSections().
    /// The group section headers are updated to record the location and and size of this data.
    void writeGroupSections (raw_ostream & OS, std::size_t FirstGroupIndex);
};

template <class ELFT>
void ELFState<ELFT>::initELFHeader (Elf_Ehdr & Header) {
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
    Header.e_machine = EM_X86_64; // FIXME: where do we represent the machine type?
    Header.e_version = EV_CURRENT;
    Header.e_entry = 0;
    Header.e_phoff = 0;
    Header.e_shoff = 0; // patched up later
    Header.e_flags = 0;
    Header.e_ehsize = sizeof (Elf_Ehdr);
    Header.e_phentsize = sizeof (Elf_Phdr);
    Header.e_phnum = 0;
    Header.e_shentsize = sizeof (Elf_Shdr);
    Header.e_shnum = 0; // patched up later.
    Header.e_shstrndx = SectionIndices::SectionNamesStrTab;
}

template <typename ELFT>
void ELFState<ELFT>::initStandardSections () {
    // null section
    Elf_Shdr SH;
    zero (SH);
    assert (SectionHeaders.size () == SectionIndices::Null);
    SectionHeaders.push_back (SH);

    // Section name string table
    zero (SH);
    SH.sh_name = SectionNames.insert (".shstrtab");
    SH.sh_type = ELF::SHT_STRTAB;
    assert (SectionHeaders.size () == SectionIndices::SectionNamesStrTab);
    SectionHeaders.push_back (SH);

    // Symbol name string table
    zero (SH);
    SH.sh_name = SectionNames.insert (".strtab");
    SH.sh_type = ELF::SHT_STRTAB;
    assert (SectionHeaders.size () == SectionIndices::SymbolNamesStrTab);
    SectionHeaders.push_back (SH);

    // Symbol table
    zero (SH);
    SH.sh_name = SectionNames.insert (".symtab");
    SH.sh_type = ELF::SHT_SYMTAB;
    SH.sh_link = SectionIndices::SymbolNamesStrTab;
    // FIXME: st_info should be one greater than the symbol table index of the last local symbol
    // (binding STB_LOCAL).
    SH.sh_info = 1;
    SH.sh_entsize = sizeof (ELFState<ELFT>::Elf_Sym);
    assert (SectionHeaders.size () == SectionIndices::SymTab);
    SectionHeaders.push_back (SH);
}

template <typename ELFT>
uint64_t ELFState<ELFT>::writeSectionHeaders (raw_ostream & OS) {
    writeAlignmentPadding<Elf_Shdr> (OS);
    auto const Offset = OS.tell ();
    OS.write (reinterpret_cast<char const *> (SectionHeaders.data ()),
              SectionHeaders.size () * sizeof (Elf_Shdr));
    return Offset;
}

// The ELF spec. requires the groups sections to appear before the sections that they contain in
// the section header table. For this reason, we have to create them in two passes: the first
// creates the headers; later, once we know the collections of thsection indices that they'll
// contain, we can write the group section bodies.
template <typename ELFT>
std::size_t ELFState<ELFT>::buildGroupSections () {
    auto const FirstGroupIndex = SectionHeaders.size ();
    for (auto const & G : Groups) {
        ELFState<ELFT>::Elf_Shdr SH;
        zero (SH);
        SH.sh_name = SectionNames.insert (".group");
        SH.sh_type = ELF::SHT_GROUP;
        SH.sh_link = SectionIndices::SymTab;
        SH.sh_info = Symbols.insertSymbol (G.first); // The group's signature symbol entry.
        SH.sh_entsize = sizeof (ELF::Elf32_Word);
        SectionHeaders.push_back (SH);
    }
    return FirstGroupIndex;
}

template <typename ELFT>
void ELFState<ELFT>::writeGroupSections (raw_ostream & OS, std::size_t ThisGroupIndex) {
    for (auto const & G : Groups) {
        writeAlignmentPadding<Elf_Word> (OS);
        auto const StartPos = OS.tell ();
        writeRaw (OS, Elf_Word{ELF::GRP_COMDAT});
        for (OutputSection<ELFT> const * GroupMember : G.second) {
            auto SectionIndex = GroupMember->getIndex ();
            writeRaw (OS, Elf_Word{SectionIndex});
            if (GroupMember->relocationsSize () > 0) {
                writeRaw (OS, Elf_Word{SectionIndex + 1});
            }
        }

        auto const SectionSize = (G.second.size () + 1) * sizeof (ELF::Elf32_Word);
        assert (OS.tell () - StartPos == SectionSize);

        Elf_Shdr & SH = SectionHeaders[ThisGroupIndex];
        assert (SH.sh_type == ELF::SHT_GROUP);
        SH.sh_offset = StartPos;
        SH.sh_size = SectionSize;

        ++ThisGroupIndex;
    }
}

LLVM_ATTRIBUTE_NORETURN static void error (Twine Message) {
    errs () << Message << "\n";
    exit (EXIT_FAILURE);
}


int main (int argc, char * argv[]) {
    cl::ParseCommandLineOptions (argc, argv);

    std::error_code EC;
    std::unique_ptr<ToolOutputFile> Out (new ToolOutputFile (OutputFilename, EC, sys::fs::F_None));
    if (EC) {
        error ("repo2obj: Error opening '" + OutputFilename + "': " + EC.message ());
    }

    ErrorOr<pstore::uuid> UuidOrError = getTicketFileUuid (TicketPath);
    if (!UuidOrError) {
        errs () << "Error: '" << TicketPath << "' (" << UuidOrError.getError ().message () << ")\n";
        return EXIT_FAILURE;
    }

    pstore::uuid const & Uuid = UuidOrError.get ();
    DEBUG (dbgs () << "'" << TicketPath << "' : " << Uuid.str () << '\n');


    pstore::database Db (RepoPath, pstore::database::access_mode::read_only);
    pstore::index::ticket_index const * const TicketIndex = Db.get_ticket_index ();
    if (!TicketIndex) {
        errs () << "Error: ticket index was not found.\n";
        return EXIT_FAILURE;
    }
    pstore::index::digest_index const * const FragmentIndex =
        Db.get_digest_index (); // FIXME: this should be called "get_fragment_index".
    if (!FragmentIndex) {
        errs () << "Error: fragment index was not found.\n";
        return EXIT_FAILURE;
    }

    auto TicketPos = TicketIndex->find (UuidOrError.get ());
    if (TicketPos == TicketIndex->end ()) {
        errs () << "Error: ticket " << Uuid.str () << " was not found.\n";
        return EXIT_FAILURE;
    }

    using ELFT = ELF64LE;
    ELFState<ELF64LE> State (Db);

    {
        auto Ticket = pstore::repo::ticket::load (Db, TicketPos->second);
        for (auto const & TM : *Ticket) {
            DEBUG (dbgs () << "Processing: " << getString (Db, TM.name) << '\n');

            auto const FragmentPos = FragmentIndex->find (TM.digest);
            if (FragmentPos == FragmentIndex->end ()) {
                errs () << "Error: fragment {" << TM.digest.high () << ',' << TM.digest.low ()
                        << "} was not found.\n";
                return EXIT_FAILURE;
            }

            auto const IsLinkOnce = (TM.linkage == pstore::repo::linkage_type::linkonce);
            auto const Discriminator = IsLinkOnce ? TM.name : pstore::address::null ();
            auto Fragment = pstore::repo::fragment::load (Db, FragmentPos->second);
            for (auto const Key : Fragment->sections ().get_indices ()) {
                // The symbol type and "discriminator" together identify the ELF output section
                // to which this fragment's section data will be appended.
                auto const Type = static_cast<pstore::repo::section_type> (Key);
                auto const Id = std::make_tuple (Type, Discriminator);

                decltype (State.Sections)::iterator Pos;
                bool DidInsert;
                std::tie (Pos, DidInsert) = State.Sections.emplace (
                    std::make_pair (Id, OutputSection<ELFT> (Db, Id, IsLinkOnce)));
                if (DidInsert && IsLinkOnce) {
                    State.Groups[TM.name].push_back (&Pos->second);
                }

                pstore::repo::section const & Section = (*Fragment)[Type];
                Pos->second.append (
                    TM, SectionPtr{std::static_pointer_cast<void const> (Fragment), &Section},
                    State.Symbols);
            }
        }
    }

    DEBUG (dbgs () << "There are " << State.Groups.size () << " groups\n");

    // FIXME: sort the symbol table.

    decltype (State)::Elf_Ehdr Header;
    State.initELFHeader (Header);
    State.initStandardSections ();

    std::size_t const FirstGroupIndex = State.buildGroupSections ();

    auto & OS = Out->os ();
    writeRaw (OS, Header);

    for (auto & S : State.Sections) {
        S.second.write (OS, State.SectionNames, State.SectionHeaders.size (),
                        std::back_inserter (State.SectionHeaders));
    }

    State.writeGroupSections (OS, FirstGroupIndex);

    // Write the two string tables (and patch their respective section headers)
    {
        auto & S = State.SectionHeaders[SectionIndices::SectionNamesStrTab];
        std::tie (S.sh_offset, S.sh_size) = State.SectionNames.write (OS);
    }
    {
        auto & S = State.SectionHeaders[SectionIndices::SymbolNamesStrTab];
        std::tie (S.sh_offset, S.sh_size) = State.SymbolNames.write (OS);
    }
    // Now do the same for the symbol table.
    {
        auto & S = State.SectionHeaders[SectionIndices::SymTab];
        std::tie (S.sh_offset, S.sh_size) = State.Symbols.write (OS);
    }

    uint64_t SectionHeadersOffset = State.writeSectionHeaders (OS);
    Header.e_shoff = SectionHeadersOffset;
    Header.e_shnum = State.SectionHeaders.size ();
    Header.e_shstrndx = SectionIndices::SectionNamesStrTab;

    OS.seek (0);
    writeRaw (OS, Header);

    //  if (Res == 0)
    Out->keep ();

    Out->os ().flush ();

    return EXIT_SUCCESS;
}
