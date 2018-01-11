//===-- R2OELFOutputSection.h ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H
#define LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H

#include "llvm/Object/ELF.h"

#include "pstore_mcrepo/ticket.hpp"
#include "pstore/sstring_view.hpp"
#include "pstore/sstring_view_archive.hpp"

#include "R2OELFStringTable.h"
#include "R2OELFSectionType.h"
#include "R2OELFSymbolTable.h"
#include "WriteHelpers.h"

#include <array>
#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <tuple>
#include <utility>
#include <vector>

#define DEBUG_TYPE "repo2obj"

using SectionPtr = std::shared_ptr<pstore::repo::section const>;

// The same as the repo types, but with a few extras

namespace details {

    struct SectionInfo {
        template <typename StringType>
        SectionInfo (StringType && N, unsigned T, unsigned F)
                : Name{std::forward<StringType> (N)}
                , Type{T}
                , Flags{F} {}
        std::string Name;
        unsigned Type;  // sh_type value
        unsigned Flags; // sh_flags value
    };
    // FIXME: this can simply be an array. ELFSectionType is an enum of small integers.
    using SectionMap = std::map<ELFSectionType, SectionInfo>;
    extern SectionMap const SectionAttributes;

} // namespace details

/// Defines the set of standard (fixed) sections that we put in the ELF file.
enum SectionIndices { Null, SectionNamesStrTab, SymbolNamesStrTab, SymTab };

using SectionId = std::tuple<ELFSectionType, pstore::address>;

template <typename ELFT>
class OutputSection {
public:
    explicit OutputSection (pstore::database & Db, SectionId Id, bool IsLinkOnce)
            : Db_{Db}
            , Id_{std::move (Id)}
            , IsLinkOnce_{IsLinkOnce} {}
    OutputSection (OutputSection const &) = delete;
    OutputSection (OutputSection &&) noexcept = default;
    OutputSection & operator= (OutputSection const &) = delete;
    OutputSection & operator= (OutputSection &&) noexcept = default;

    std::uint64_t contributionsSize () const {
        return SectionSize_;
    }
    std::uint64_t numRelocations () const {
        return Relocations_.size ();
    }

    /// This structure is used to provide the data necessary to create a symbol which references
    /// the first byte of each section contributed by a fragment.
    struct SectionInfo {
        SectionInfo () noexcept = default;
        SectionInfo (OutputSection<ELFT> * Section, std::uint64_t Offset) noexcept
                : Section_{Section}
                , Offset_{Offset} {}

        OutputSection<ELFT> * section () {
            return Section_;
        }
        /// Returns the symbol associated with this section/offset, creating it if necessary.
        typename SymbolTable<ELFT>::Value * symbol (SymbolTable<ELFT> & Symbols);

    private:
        OutputSection<ELFT> * Section_ = nullptr;
        std::uint64_t Offset_ = 0;
        typename SymbolTable<ELFT>::Value * Symbol_ = nullptr;
    };

    void append (pstore::repo::ticket_member const & TM, SectionPtr SectionData,
                 SymbolTable<ELFT> & Symbols, std::vector<SectionInfo> & OutputSections);

    /// \returns The number of ELF sections added by this OutputSection.
    std::size_t numSections () const;

    /// Writes the section data, relocations, and creates the necessary section header table
    /// entries.
    ///
    /// \tparam OutputIt  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type.
    /// \param OS The binary output stream to which the contents of the output sections are written.
    /// \param SectionNames  The collection of section names.
    /// \param OutShdr  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type as the write function creates sections in the output.
    template <typename OutputIt>
    OutputIt write (llvm::raw_ostream & OS, StringTable & SectionNames, OutputIt OutShdr) const;

    ELFSectionType getType () const {
        return std::get<0> (Id_);
    }
    unsigned getIndex () const {
        assert (Index_ != UnknownIndex);
        return static_cast<unsigned> (Index_);
    }
    void setIndex (unsigned Index) {
        assert (Index != UnknownIndex && Index_ == UnknownIndex);
        Index_ = Index;
    }

private:
    pstore::database const & Db_;
    SectionId const Id_;
    bool const IsLinkOnce_;

    static constexpr auto UnknownIndex = std::numeric_limits<unsigned>::max ();
    unsigned Index_ = UnknownIndex; // The section header table index
    uint8_t Align_ = 0;

    // TODO: We have no a priori knowledge of the number of text contributions to this output
    // section. Using some sort of "chunked vector" might be considerably more efficient.
    std::vector<SectionPtr> Contributions_;
    std::uint64_t SectionSize_ = 0;

    using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
    using Elf_Rela = typename llvm::object::ELFFile<ELFT>::Elf_Rela;
    using Elf_Shdr = typename llvm::object::ELFFile<ELFT>::Elf_Shdr;
    using SymbolTarget = typename SymbolTable<ELFT>::SymbolTarget;

    struct Relocation {
        Relocation (typename SymbolTable<ELFT>::Value * Symbol_,
                    pstore::repo::relocation_type Type_, std::uint64_t Offset_,
                    std::uint64_t Addend_)
                : Symbol{Symbol_}
                , Type{Type_}
                , Offset{Offset_}
                , Addend{Addend_} {
            assert (Symbol != nullptr);
        }

        /// The symbol targeted by this relocation.
        typename SymbolTable<ELFT>::Value * Symbol;
        pstore::repo::relocation_type Type;
        std::uint64_t Offset;
        std::uint64_t Addend;
    };
    std::vector<Relocation> Relocations_;

    template <typename T>
    static T alignedBytes (T v, uint8_t align_shift) {
        auto align = T{1} << align_shift;
        return (align - T{1}) & ~(align - T{1});
    }

    static void writePadding (llvm::raw_ostream & OS, unsigned bytes);

    std::string dataSectionName (std::string SectionName, pstore::address DiscriminatorName) const;
    std::string relocationSectionName (std::string const & BaseName) const;
};

// FIXME: this needs to be passeed in and not hard-wired.
constexpr bool IsMips64EL = false;

template <typename ELFT>
typename SymbolTable<ELFT>::Value *
OutputSection<ELFT>::SectionInfo::symbol (SymbolTable<ELFT> & Symbols) {
    using namespace llvm;
    if (Symbol_ == nullptr) {
        static auto PrivateSymbolCount = 0U;

        auto Name = stringToSStringView (".LR" + std::to_string (PrivateSymbolCount++));
        Symbol_ = Symbols.insertSymbol (Name, Section_, Offset_, 0 /*size*/,
                                        pstore::repo::linkage_type::internal);

        DEBUG (dbgs () << "  created symbol (" << Name << ") for internal fixup (index " << Symbol_
                       << ")\n");

        assert (Symbol_ != nullptr);
    }
    return Symbol_;
}


// writePadding
// ~~~~~~~~~~~~
// FIXME: the padding data depends on the output section. In particular, the text section should
// receive NOP instructions. This is modelled in
template <typename ELFT>
void OutputSection<ELFT>::writePadding (llvm::raw_ostream & OS, unsigned bytes) {
    auto const b = uint8_t{0};
    for (auto ctr = 0U; ctr < bytes; ++ctr) {
        writeRaw (OS, b);
    }
}

// append
// ~~~~~~
template <typename ELFT>
void OutputSection<ELFT>::append (pstore::repo::ticket_member const & TM, SectionPtr SectionData,
                                  SymbolTable<ELFT> & Symbols,
                                  std::vector<SectionInfo> & OutputSections) {

    using namespace llvm;

    Contributions_.emplace_back (SectionData);

    auto const ObjectSize = SectionData->data ().size ();
    DEBUG (dbgs () << "  generating relocations FROM '" << ::getString (Db_, TM.name) << "'\n");

    std::uint8_t const DataAlign = std::uint8_t{1} << SectionData->align ();
    // ELF section alignment is the maximum of the alignment of all its contributions.
    Align_ = std::max (Align_, DataAlign);

    SectionSize_ += alignedBytes (SectionSize_, DataAlign);

    // "append" linkage is slightly unusual in that multiple definitions of the same symbol
    // simply pile up one after the other in the output. ELF doesn't have this concept, obviously,
    // so we need to ensure that we don't produce a definition of this symbol in each object file
    // that will result in an error when they are linked.
    //
    // This check is sufficient for the llvm.globl_ctors/dtors use-case where the symbols are
    // mapped to the .init_array/.fini_array sections and we don't actually need a symbol which
    // references the data.

    if (TM.linkage != pstore::repo::linkage_type::append) {
        Symbols.insertSymbol (getString (Db_, TM.name), this, SectionSize_, ObjectSize, TM.linkage);
    }

    for (pstore::repo::external_fixup const & XFixup : SectionData->xfixups ()) {
        auto const TargetName = getString (Db_, XFixup.name);
        DEBUG (dbgs () << "  generating relocation TO '" << TargetName << '\n');
        Relocations_.emplace_back (Symbols.insertSymbol (TargetName), XFixup.type,
                                   XFixup.offset + SectionSize_, XFixup.addend);
    }

    for (pstore::repo::internal_fixup const & IFixup : SectionData->ifixups ()) {
        auto const TargetSectionIndex =
            static_cast<typename std::underlying_type<decltype (IFixup.section)>::type> (
                IFixup.section);
        assert (TargetSectionIndex >= 0 && TargetSectionIndex < OutputSections.size ());
        auto & TargetSection = OutputSections[TargetSectionIndex];

        Relocations_.emplace_back (TargetSection.symbol (Symbols), IFixup.type,
                                   IFixup.offset + SectionSize_, IFixup.addend);
    }

    SectionSize_ += ObjectSize;
}

// numSections
// ~~~~~~~~~~~
template <typename ELFT>
std::size_t OutputSection<ELFT>::numSections () const {
    return 1U + static_cast<std::size_t> (Relocations_.size () > 0);
}

// write
// ~~~~~
template <typename ELFT>
template <typename OutputIt>
OutputIt OutputSection<ELFT>::write (llvm::raw_ostream & OS, StringTable & SectionNames,
                                     OutputIt OutShdr) const {
    assert (Index_ != UnknownIndex);
    auto const GroupFlag = IsLinkOnce_ ? Elf_Word (llvm::ELF::SHF_GROUP) : Elf_Word (0);

    auto const StartPos = OS.tell ();
    auto Pos = StartPos;

    for (auto const & Contribution : Contributions_) {
        pstore::repo::section::container<std::uint8_t> D = Contribution->data ();

        auto const Alignment = alignedBytes (Pos, Contribution->align ());
        this->writePadding (OS, Alignment);
        OS.write (reinterpret_cast<char const *> (D.data ()), D.size ());
        Pos += Alignment + D.size ();
    }
    assert (OS.tell () - StartPos == SectionSize_);

    auto const & Attrs = details::SectionAttributes.find (this->getType ());
    assert (Attrs != details::SectionAttributes.end ());
    std::string const SectionName =
        this->dataSectionName (Attrs->second.Name, std::get<1> (Id_) /*Discriminator*/);
    {
        DEBUG (llvm::dbgs () << "section " << SectionName << " index " << Index_ << '\n');

        Elf_Shdr SH;
        zero (SH);
        SH.sh_name = SectionNames.insert (stringToSStringView (SectionName));
        SH.sh_type = Attrs->second.Type;
        SH.sh_flags = Attrs->second.Flags | GroupFlag;
        SH.sh_offset = StartPos;
        SH.sh_size = SectionSize_;
        SH.sh_addralign = Align_;
        *(OutShdr++) = SH;
    }

    if (Relocations_.size () > 0) {
        writeAlignmentPadding<Elf_Rela> (OS);
        auto const RelaStartPos = OS.tell ();

        for (Relocation const & R : Relocations_) {
            Elf_Rela Rel;
            auto SymbolIndex = R.Symbol->Index;
            Rel.setSymbolAndType (SymbolIndex, R.Type, IsMips64EL);
            Rel.r_offset = R.Offset;
            Rel.r_addend = R.Addend;
            OS.write (reinterpret_cast<char const *> (&Rel), sizeof (Rel));
        }

        Elf_Shdr RelaSH;
        zero (RelaSH);
        RelaSH.sh_name =
            SectionNames.insert (stringToSStringView (this->relocationSectionName (SectionName)));
        RelaSH.sh_type = llvm::ELF::SHT_RELA;
        RelaSH.sh_flags =
            llvm::ELF::SHF_INFO_LINK | GroupFlag; // sh_info holds index of the target section.
        RelaSH.sh_offset = RelaStartPos;
        RelaSH.sh_size = Relocations_.size () * sizeof (Elf_Rela);
        RelaSH.sh_link = SectionIndices::SymTab;
        RelaSH.sh_info = Index_; // target section
        RelaSH.sh_entsize = sizeof (Elf_Rela);
        RelaSH.sh_addralign = alignof (Elf_Rela);
        *(OutShdr++) = RelaSH;
    }
    return OutShdr;
}

// dataSectionName
// ~~~~~~~~~~~~~~~
template <typename ELFT>
std::string OutputSection<ELFT>::dataSectionName (std::string SectionName,
                                                  pstore::address DiscriminatorName) const {
    if (DiscriminatorName != pstore::address::null ()) {
        SectionName += '.';
        SectionName += getString (Db_, DiscriminatorName).to_string ();
    }
    return SectionName;
}

// relocationSectionName
// ~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
std::string OutputSection<ELFT>::relocationSectionName (std::string const & BaseName) const {
    if (BaseName[0] == '.') {
        return std::string{".rela"} + BaseName;
    }
    return std::string{".rela."} + BaseName;
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H
