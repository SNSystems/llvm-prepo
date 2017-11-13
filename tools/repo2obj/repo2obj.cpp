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
#include "llvm/Object/ELFTypes.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ToolOutputFile.h"

#include "pstore/database.hpp"
#include "pstore/hamt_map.hpp"
#include "pstore_mcrepo/fragment.hpp"
#include "pstore_mcrepo/ticket.hpp"

#include "R2OELFOutputSection.h"
#include "WriteHelpers.h"

#include <cstdlib>
#include <functional>
#include <unordered_map>
#include <list>
#include <tuple>
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
    typedef typename object::ELFFile<ELFT>::Elf_Ehdr Elf_Ehdr;
    typedef typename object::ELFFile<ELFT>::Elf_Phdr Elf_Phdr;
    typedef typename object::ELFFile<ELFT>::Elf_Shdr Elf_Shdr;
    typedef typename object::ELFFile<ELFT>::Elf_Sym Elf_Sym;
    typedef typename object::ELFFile<ELFT>::Elf_Rel Elf_Rel;
    typedef typename object::ELFFile<ELFT>::Elf_Rela Elf_Rela;

    using SectionHeaderTable = std::vector<Elf_Shdr>;
    SectionHeaderTable SectionHeaders;

    // using SectionNameStringTable = StringTable<std::string>;
    SectionNameStringTable SectionNames;

    void initELFHeader (Elf_Ehdr & Header);
    void initStandardSections ();
};

// using ELFT = ELF64LE;
// template struct ELFState<ELFT>;

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
    Header.e_type = ET_REL;       // Doc.Header.Type;
    Header.e_machine = EM_X86_64; /// ;; Doc.Header.Machine;
    Header.e_version = EV_CURRENT;
    Header.e_entry = 0; // Doc.Header.Entry;
    Header.e_phoff = 0;
    Header.e_shoff = 0; // patched up later
    Header.e_flags = 0;
    Header.e_ehsize = sizeof (Elf_Ehdr);
    Header.e_phentsize = sizeof (Elf_Phdr);
    Header.e_phnum = 0;
    Header.e_shentsize = sizeof (Elf_Shdr);
    Header.e_shnum = 0;                                     // patched up later.
    Header.e_shstrndx = SectionIndices::SectionNamesStrTab; // getDotShStrTabSecNo();//FIXME:
}

template <typename ELFT>
void ELFState<ELFT>::initStandardSections () {
    // Null section
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
    // (binding STRB_LOCAL).
    SH.sh_entsize = sizeof (ELFT::Elf_Sym);
    assert (SectionHeaders.size () == SectionIndices::SymTab);
    SectionHeaders.push_back (SH);
}



using SymbolNameStringTable = StringTable<pstore::address>;



// template <typename ELFT>
// class SymbolTable



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
    outs () << "'" << TicketPath << "' : " << Uuid.str () << '\n';


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
    ELFState<ELFT> State;

    SymbolNameStringTable SymbolNames{StringPolicy<pstore::address> (Db)};
    SymbolTable<ELFT> Symbols (SymbolNames);


    std::vector<OutputSection<ELFT>> Sections;
    std::map<pstore::repo::section_type, unsigned> SectionMap;

    {
        std::shared_ptr<pstore::repo::ticket const> Ticket =
            pstore::repo::ticket::load (Db, TicketPos->second);
        for (auto const & TM : *Ticket) {
            // add "name" to the symbol table

            auto const FragmentPos = FragmentIndex->find (TM.digest);
            if (FragmentPos == FragmentIndex->end ()) {
                errs () << "Error: fragment {" << TM.digest.high () << ',' << TM.digest.low ()
                        << "} was not found.\n";
                return EXIT_FAILURE;
            }

            std::shared_ptr<pstore::repo::fragment const> Fragment =
                pstore::repo::fragment::load (Db, FragmentPos->second);
            for (auto const Key : Fragment->sections ().get_indices ()) {
                auto const Type = static_cast<pstore::repo::section_type> (Key);

                decltype (SectionMap)::iterator Pos;
                bool DidInsert;
                std::size_t const NumSections = Sections.size ();
                std::tie (Pos, DidInsert) = SectionMap.emplace (Type, NumSections);

                std::size_t Index = 0U;
                if (DidInsert) {
                    // First time we've seen a contribution to this output section.
                    Sections.emplace_back (Db, Type, NumSections + SectionIndices::last);
                    Index = NumSections;
                } else {
                    Index = Pos->second;
                }

                dbgs () << "Processing: " << getString (Db, TM.name) << '\n';
                pstore::repo::section const & Section = (*Fragment)[Type];
                Sections[Index].append (
                    TM, Type, SectionPtr{std::static_pointer_cast<void const> (Fragment), &Section},
                    Symbols);
            }
        }
    }

    // sort the symbol table

    ELFState<ELFT>::Elf_Ehdr Header;
    State.initELFHeader (Header);

    State.SectionHeaders.reserve (SectionIndices::last + Sections.size () * 2);
    {
        // null section
        ELFState<ELFT>::Elf_Shdr SH;
        zero (SH);
        assert (State.SectionHeaders.size () == SectionIndices::Null);
        State.SectionHeaders.push_back (SH);

        // Section name string table
        zero (SH);
        SH.sh_name = State.SectionNames.insert (".shstrtab");
        SH.sh_type = ELF::SHT_STRTAB;
        assert (State.SectionHeaders.size () == SectionIndices::SectionNamesStrTab);
        State.SectionHeaders.push_back (SH);

        // Symbol name string table
        zero (SH);
        SH.sh_name = State.SectionNames.insert (".strtab");
        SH.sh_type = ELF::SHT_STRTAB;
        assert (State.SectionHeaders.size () == SectionIndices::SymbolNamesStrTab);
        State.SectionHeaders.push_back (SH);

        // Symbol table
        zero (SH);
        SH.sh_name = State.SectionNames.insert (".symtab");
        SH.sh_type = ELF::SHT_SYMTAB;
        SH.sh_link = SectionIndices::SymbolNamesStrTab;
        // FIXME: st_info should be one greater than the symbol table index of the last local symbol
        // (binding STRB_LOCAL).
        SH.sh_entsize = sizeof (ELFState<ELFT>::Elf_Sym);
        assert (State.SectionHeaders.size () == SectionIndices::SymTab);
        State.SectionHeaders.push_back (SH);
    }

    auto & OS = Out->os ();
    writeRaw (OS, Header);
    for (auto const & S : Sections) {
        S.write (OS, State.SectionNames, State.SectionHeaders.size (),
                 std::back_inserter (State.SectionHeaders));
    }

    // Write the two string tables (and patch their respective section headers)
    {
        auto & S = State.SectionHeaders[SectionIndices::SectionNamesStrTab];
        std::tie (S.sh_offset, S.sh_size) = State.SectionNames.write (OS);
    }
    {
        auto & S = State.SectionHeaders[SectionIndices::SymbolNamesStrTab];
        std::tie (S.sh_offset, S.sh_size) = SymbolNames.write (OS);
    }
    {
        auto & S = State.SectionHeaders[SectionIndices::SymTab];
        std::tie (S.sh_offset, S.sh_size) = Symbols.write (OS);
    }

    {
        writeAlignmentPadding<ELFState<ELFT>::Elf_Shdr> (OS);
        Header.e_shoff = OS.tell ();
        Header.e_shnum = State.SectionHeaders.size ();
        OS.write (reinterpret_cast<char const *> (State.SectionHeaders.data ()),
                  State.SectionHeaders.size () * sizeof (ELFState<ELFT>::Elf_Shdr));
    }

    OS.seek (0);
    writeRaw (OS, Header);

    //  if (Res == 0)
    Out->keep ();

    Out->os ().flush ();

    return EXIT_SUCCESS;
}
