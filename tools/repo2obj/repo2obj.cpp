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

#include <cstdlib>
#include <functional>
#include <unordered_map>
#include <list>
#include <tuple>
#ifdef _WIN32
#include <io.h>
#endif

// FIXME: remove
pstore::database * Db_ = nullptr;

using namespace llvm;
using namespace llvm::object;

namespace {

    cl::opt<std::string> RepoPath (cl::Optional, cl::desc ("Program pepository path"),
                                   cl::init ("./clang.db"));
    cl::opt<std::string> TicketPath (cl::Positional, cl::desc ("<ticket path>"));
    static cl::opt<std::string> OutputFilename ("o", cl::desc ("Output filename"),
                                                cl::value_desc ("filename"));
} // namespace


template <typename T, typename = typename std::enable_if<std::is_standard_layout<T>::value>::type>
static void zero (T & t) {
    std::memset (&t, 0, sizeof (T));
}

template <typename T, typename = typename std::enable_if<std::is_standard_layout<T>::value>::type>
static void writeRaw (raw_ostream & OS, T const & t) {
    OS.write (reinterpret_cast<char const *> (&t), sizeof (t));
}

template <typename T>
void writeAlignmentPadding (raw_ostream & OS) {
    constexpr auto Alignment = alignof (T);
    uint8_t Padding[Alignment] = {0};
    OS.write (reinterpret_cast<char const *> (Padding), Alignment - (OS.tell () % Alignment));
}

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


namespace {

    std::string getString (pstore::database const & db, pstore::address addr) {
        using namespace pstore::serialize;
        archive::database_reader source (db, addr);
        return read<std::string> (source);
    }

} // namespace

template <typename T>
struct StringPolicy {
    std::string get (T const & Str) const;
    std::size_t length (T const & Str) const;
};

template <>
struct StringPolicy<std::string> {
    std::string get (std::string const & Str) const {
        return Str;
    }
    std::size_t length (std::string const & Str) const {
        return Str.length ();
    }
};

template <>
class StringPolicy<pstore::address> {
public:
    explicit StringPolicy (pstore::database const & Db)
            : Db_{Db} {}

    std::string get (pstore::address Addr) const {
        return getString (Db_, Addr);
    }
    std::size_t length (pstore::address Addr) const {
        return getString (Db_, Addr).length ();
    }

private:
    pstore::database const & Db_;
};


template <typename T, typename Policy = StringPolicy<T>>
class StringTable {
    using container = std::unordered_map<T, std::uint64_t>;

public:
    using value_type = typename container::value_type;
    using iterator = typename container::const_iterator;
    using const_iterator = iterator;

    static constexpr auto npos = std::numeric_limits<std::uint64_t>::max ();

    explicit StringTable (Policy const & P = Policy ())
            : Policy_{P} {}

    const_iterator begin () const {
        return std::begin (Strings_);
    }
    const_iterator end () const {
        return std::end (Strings_);
    }

    /// Inserts a name into the string table if not already present and returns the string table
    /// offset for the string.
    std::uint64_t insert (T const & Name);
    std::uint64_t position (T const & Name) const;

    std::tuple<std::uint64_t, std::uint64_t> write (raw_ostream & OS) const;

    std::uint64_t dataSize () const {
        return DataSize_;
    }

private:
    Policy Policy_;
    container Strings_;
    std::vector<T> Data_;
    std::uint64_t DataSize_ = 1U;
};

template <typename T, typename Traits>
std::uint64_t StringTable<T, Traits>::insert (T const & Name) {
    typename container::iterator Pos;
    bool DidInsert;
    std::tie (Pos, DidInsert) = Strings_.emplace (Name, 0);
    if (DidInsert) {
        Pos->second = DataSize_;
        DataSize_ += Policy_.length (Name) + 1;
        Data_.push_back (Name);
    }
    return Pos->second;
}

template <typename T, typename Traits>
std::uint64_t StringTable<T, Traits>::position (T const & Name) const {
    auto Pos = Strings_.find (Name);
    return Pos != Strings_.end () ? Pos->second : npos;
}

template <typename T, typename Traits>
std::tuple<std::uint64_t, std::uint64_t> StringTable<T, Traits>::write (raw_ostream & OS) const {
    std::uint64_t Start = OS.tell ();
    OS << '\0';
    for (T const & Name : Data_) {
        assert (OS.tell () - Start == this->position (Name));
        OS << Policy_.get (Name) << '\0';
    }
    std::uint64_t End = OS.tell ();
    assert (End >= Start);
    return {Start, End - Start};
}

namespace std {
    template <>
    struct hash<pstore::address> {
        using argument_type = pstore::address;
        using result_type = std::size_t;

        result_type operator() (argument_type s) const {
            auto abs = s.absolute ();
            return std::hash<decltype (abs)>{}(abs);
        }
    };
} // namespace std


/// Defines the set of standard (fixed) sections that we put in the ELF file.
enum SectionIndices { Null, SectionNamesStrTab, SymbolNamesStrTab, SymTab, last };


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

    using SectionNameStringTable = StringTable<std::string>;
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



template <typename ELFT>
class SymbolTable;


using SectionPtr = std::shared_ptr<pstore::repo::section const>;

template <typename ELFT>
class OutputSection {
public:
    explicit OutputSection (pstore::repo::section_type T, unsigned Index)
            : Type_{T}
            , Index_{Index} {}

    std::uint64_t contributionsSize () const {
        return SectionSize_;
    }
    std::uint64_t relocationsSize () const {
        return Relocations_.size () * sizeof (typename ELFState<ELFT>::Elf_Rela);
    }

    void append (pstore::repo::ticket_member const & TM, pstore::repo::section_type SectionType,
                 SectionPtr SectionData, SymbolTable<ELFT> & Symbols);
    void write (raw_ostream & OS, typename ELFState<ELFT>::SectionNameStringTable & SectionNames,
                typename ELFState<ELFT>::SectionHeaderTable & Sections) const;

    pstore::repo::section_type getType () const {
        return Type_;
    }
    unsigned getIndex () const {
        return Index_;
    }

private:
    pstore::repo::section_type const Type_;
    unsigned const Index_; // The section header table index
    std::vector<SectionPtr> Contributions_;
    std::uint64_t SectionSize_ = 0;
    std::vector<typename ELFState<ELFT>::Elf_Rela> Relocations_;
};



template <typename ELFT>
class SymbolTable {
public:
    explicit SymbolTable (SymbolNameStringTable & Strings)
            : Strings_{Strings} {}
    SymbolTable (SymbolTable const &) = delete;
    SymbolTable & operator= (SymbolTable const &) = delete;

    struct Target {
        Target (unsigned SectionIndex_, pstore::repo::section_type SectionType_,
                std::uint64_t Offset_, pstore::repo::linkage_type Linkage_)
                : SectionIndex{SectionIndex_}
                , SectionType{SectionType_}
                , Offset{Offset_}
                , Linkage{Linkage_} {}

        unsigned SectionIndex;
        pstore::repo::section_type SectionType;

        std::uint64_t Offset;
        pstore::repo::linkage_type Linkage;
    };

    std::uint64_t insertSymbol (pstore::address Name, llvm::Optional<Target> const & Target);

    /// Creates a definition in the symbol table.
    std::uint64_t insertSymbol (pstore::address Name, unsigned SectionIndex,
                                pstore::repo::section_type Type, std::uint64_t Offset,
                                pstore::repo::linkage_type Linkage) {
        return this->insertSymbol (Name, Target{SectionIndex, Type, Offset, Linkage});
    }

    // If not already in the symbol table, an undef entry is created. This may be later turned into
    // a proper definition.
    std::uint64_t insertSymbol (pstore::address Name) {
        return this->insertSymbol (Name, None);
    }

    std::tuple<std::uint64_t, std::uint64_t> write (raw_ostream & OS);

private:
    static unsigned linkageToELFBinding (pstore::repo::linkage_type L);

    struct Value {
        std::size_t SymbolIndex;
        std::uint64_t NameOffset;
        llvm::Optional<Target> Target;
    };
    // FIXME: this pair of fields is replicated for the section table. Refactoring?
    std::vector<Value> Symbols_;
    std::unordered_map<pstore::address, uint64_t> SymbolMap_;
    SymbolNameStringTable & Strings_;
};

template <typename ELFT>
unsigned SymbolTable<ELFT>::linkageToELFBinding (pstore::repo::linkage_type L) {
#if 1
    unsigned Binding = ELF::STB_GLOBAL;
#else
    unsigned Binding;
    switch (T.Linkage) {
    case pstore::repo::linkage_type::external:
    case pstore::repo::linkage_type::common:
    case pstore::repo::linkage_type::linkonce:
        Binding = ELF::STB_GLOBAL;
        break;
    case pstore::repo::linkage_type::internal:
        Binding = ELF::STB_LOCAL;
        break;
    }
#endif
    return Binding;
}

template <typename ELFT>
std::uint64_t SymbolTable<ELFT>::insertSymbol (pstore::address Name,
                                               llvm::Optional<Target> const & Target) {
    typename decltype (SymbolMap_)::iterator Pos;
    bool DidInsert;
    std::tie (Pos, DidInsert) = SymbolMap_.emplace (Name, 0);
    if (DidInsert) {
        uint64_t const SymbolIndex = SymbolMap_.size ();
        Pos->second = SymbolIndex;
        Symbols_.push_back (Value{SymbolIndex, Strings_.insert (Name), Target});
        return SymbolIndex;
    }

    // If we don't have a value associated with this symbol, then use the one we have here.
    // FIXME: if Target && V.Target, we're attemptting to make a duplicate definition.
    uint64_t const SymbolIndex = Pos->second;
    Value & V = Symbols_[SymbolIndex];
    if (!V.Target) {
        V.Target = Target;
    }
    return SymbolIndex;
}

template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t> SymbolTable<ELFT>::write (raw_ostream & OS) {
    writeAlignmentPadding<typename ELFState<ELFT>::Elf_Sym> (OS);

    uint64_t StartOffset = OS.tell ();

    typename ELFState<ELFT>::Elf_Sym Symbol;
    // Write the reserved zeroth symbol table entry.
    zero (Symbol);
    Symbol.st_shndx = ELF::SHN_UNDEF;
    writeRaw (OS, Symbol);

    for (Value const & SV : Symbols_) {
        zero (Symbol);
        Symbol.st_name = SV.NameOffset;

        if (SV.Target) {
            Target const & T = SV.Target.getValue ();
            Symbol.st_value = T.Offset;

            unsigned const Binding = linkageToELFBinding (T.Linkage);
            auto const SymbolType = (T.SectionType == pstore::repo::section_type::Text)
                                        ? ELF::STT_FUNC
                                        : ELF::STT_OBJECT;
            Symbol.setBindingAndType (Binding, SymbolType);
            Symbol.st_shndx =
                T.SectionIndex; // The section (header table index) in which this value is defined.
        } else {
            Symbol.setBindingAndType (ELF::STB_GLOBAL, ELF::STT_NOTYPE);
            Symbol.st_shndx = ELF::SHN_UNDEF; // There's no definition for this name.
        }
        Symbol.st_size = 0; // FIXME: this code has no idea what the size is (yet).

        writeRaw (OS, Symbol);
    }
    uint64_t EndOffset = OS.tell ();
    return {StartOffset, EndOffset - StartOffset};
}


using SectionMap = std::map<pstore::repo::section_type,
                            std::tuple<char const *, unsigned /*sh_type*/, unsigned /*sh_flags*/>>;
SectionMap const SectionAttributes{
    // X (BSS)
    // X (Common)
    // X (Data)
    std::make_pair (pstore::repo::section_type::Data,
                    std::make_tuple (".data", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE)),
    // X (RelRo)
    std::make_pair (
        pstore::repo::section_type::Text,
        std::make_tuple (".text", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_EXECINSTR)),
    // X (Mergeable1ByteCString)
    // X (Mergeable2ByteCString)
    // X (Mergeable4ByteCString)
    // X (MergeableConst4)
    // X (MergeableConst8)
    // X (MergeableConst16)
    // X (MergeableConst32)
    // X (MergeableConst)
    std::make_pair (pstore::repo::section_type::ReadOnly,
                    std::make_tuple (".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC)),
    // X (ThreadBSS)
    // X (ThreadData)
    // X (ThreadLocal)
    // X (Metadata)
};

constexpr bool IsMips64EL = false;


template <typename ELFT>
void OutputSection<ELFT>::append (pstore::repo::ticket_member const & TM,
                                  pstore::repo::section_type SectionType, SectionPtr SectionData,
                                  SymbolTable<ELFT> & Symbols) {
    Contributions_.emplace_back (SectionData);
    // FIXME: this assumes that the section represents the fragment's one and only section.
    // When we begin to generate multi-section fragments, this will need to be a little
    // more sophisticated. We'll also need to be able to add non-database-resident names
    // to the symbol table.
    dbgs() << "generating relocations FROM " << getString (*Db_, TM.name) << '\n';
    Symbols.insertSymbol (
        TM.name, typename SymbolTable<ELFT>::Target{Index_, SectionType, SectionSize_, TM.linkage});

    auto const InitialSectionSize = SectionSize_;
    SectionSize_ += SectionData->data ().size (); // TODO: account for alignment

    auto const & XFixups = SectionData->xfixups ();
    Relocations_.reserve (Relocations_.size () + XFixups.size ());
    for (pstore::repo::external_fixup const & Fixup : XFixups) {
        ELF64LE::Rela Reloc;
        auto const SymbolIndex = Symbols.insertSymbol (Fixup.name);
        dbgs() << "generating relocation TO '" << getString (*Db_, Fixup.name) << "' symbol index " << SymbolIndex << '\n';
        Reloc.setSymbolAndType (SymbolIndex, Fixup.type, IsMips64EL);
        Reloc.r_offset = Fixup.offset + InitialSectionSize;
        Reloc.r_addend = Fixup.addend;
        Relocations_.push_back (Reloc);
    }

    // TODO: the internal fixups.
}

template <typename ELFT>
void OutputSection<ELFT>::write (raw_ostream & OS,
                                 typename ELFState<ELFT>::SectionNameStringTable & SectionNames,
                                 typename ELFState<ELFT>::SectionHeaderTable & Sections) const {
    auto const StartPos = OS.tell ();
    for (auto const & Contribution : Contributions_) {
        pstore::repo::section::container<std::uint8_t> D = Contribution->data ();
        // TODO: alignment.
        OS.write (reinterpret_cast<char const *> (D.data ()), D.size ());
    }
    assert (OS.tell () - StartPos == SectionSize_);

    auto const & Attrs = SectionAttributes.find (Type_);
    assert (Attrs != SectionAttributes.end ());
    auto const SectionName = std::get<0> (Attrs->second);
    {
        typename ELFState<ELFT>::Elf_Shdr SH;
        zero (SH);
        SH.sh_name = SectionNames.insert (SectionName);
        SH.sh_type = std::get<1> (Attrs->second);
        SH.sh_flags = std::get<2> (Attrs->second);
        SH.sh_offset = StartPos;
        SH.sh_size = SectionSize_;
        Sections.push_back (SH);
    }

    if (Relocations_.size () > 0) {
        auto const RelaStartPos = StartPos + SectionSize_;
        assert (RelaStartPos == OS.tell ());
        auto const RelocsSize = this->relocationsSize ();
        OS.write (reinterpret_cast<char const *> (Relocations_.data ()), RelocsSize);
        {
            typename ELFState<ELFT>::Elf_Shdr RelaSH;
            zero (RelaSH);
            RelaSH.sh_name = SectionNames.insert (std::string{".rela."} + SectionName);
            RelaSH.sh_type = ELF::SHT_RELA;
            RelaSH.sh_flags = ELF::SHF_INFO_LINK; // sh_info holds index of the target section.
            RelaSH.sh_offset = RelaStartPos;
            RelaSH.sh_size = RelocsSize;
            RelaSH.sh_link = SectionIndices::SymTab;
            RelaSH.sh_info = Sections.size () - 1; // target
            RelaSH.sh_entsize = sizeof (typename ELFState<ELFT>::Elf_Rela);
            Sections.push_back (RelaSH);
        }
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
    outs () << "'" << TicketPath << "' : " << Uuid.str () << '\n';


    pstore::database Db (RepoPath, pstore::database::access_mode::read_only);
    Db_ = &Db;
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
                    Sections.emplace_back (Type, NumSections + SectionIndices::last);
                    Index = NumSections;
                } else {
                    Index = Pos->second;
                }

                dbgs() << "Processing: " << getString (Db, TM.name) << '\n';
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
        S.write (OS, State.SectionNames, State.SectionHeaders);
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
