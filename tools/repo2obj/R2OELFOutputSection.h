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

#include "pstore_mcrepo/fragment.hpp"
#include "pstore_mcrepo/ticket.hpp"

#include "R2OELFSymbolTable.h"
#include "WriteHelpers.h"

#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <utility>
#include <vector>

#define DEBUG_TYPE "repo2obj"

using SectionPtr = std::shared_ptr<pstore::repo::section const>;

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
    // FIXME: this can simply be an array. section_type is an enum of small integers.
    using SectionMap = std::map<pstore::repo::section_type, SectionInfo>;
    extern SectionMap const SectionAttributes;

} // namespace details

/// Defines the set of standard (fixed) sections that we put in the ELF file.
enum SectionIndices { Null, SectionNamesStrTab, SymbolNamesStrTab, SymTab };

using SectionId = std::tuple<pstore::repo::section_type, pstore::address>;

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
    std::uint64_t relocationsSize () const {
        return Relocations_.size () * sizeof (Elf_Rela);
    }

    void append (pstore::repo::ticket_member const & TM, SectionPtr SectionData,
                 SymbolTable<ELFT> & Symbols);

    /// \returns The number of ELF sections added by this OutputSection.
    std::size_t numSections () const;

    /// Writes the section data, relocations, and creates the necessary section header table
    /// entries.
    ///
    /// \tparam OutputIt  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type.
    /// \param OS The binary output stream to which the contents of the output sections are written.
    /// \param SectionNames
    /// \param OutShdr  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type as the write function creates sections in the output.
    template <typename OutputIt>
    OutputIt write (llvm::raw_ostream & OS, SectionNameStringTable & SectionNames,
                    OutputIt OutShdr) const;

    pstore::repo::section_type getType () const {
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

    std::vector<Elf_Rela> Relocations_;

    template <typename T>
    static T alignedBytes (T v, uint8_t align_shift) {
        auto align = T{1} << align_shift;
        return (align - T{1}) & ~(align - T{1});
    }

    static void writePadding (llvm::raw_ostream & OS, unsigned bytes);

    std::string dataSectionName (std::string SectionName, pstore::address DiscriminatorName) const;
    std::string relocationSectionName (std::string const & BaseName) const;

    std::string getString (pstore::address Addr) const;
};

// FIXME: this needs to be passeed in and not hard-wired.
constexpr bool IsMips64EL = false;


template <typename ELFT>
std::string OutputSection<ELFT>::getString (pstore::address Addr) const {
    using namespace pstore::serialize;
    archive::database_reader Source (Db_, Addr);
    return read<std::string> (Source);
}

// FIXME: the padding data depends on the output section. In particular, the text section should
// receive NOP instructions. This is modelled in 
template <typename ELFT>
void OutputSection<ELFT>::writePadding (llvm::raw_ostream & OS, unsigned bytes) {
    auto const b = uint8_t{0};
    for (auto ctr = 0U; ctr < bytes; ++ctr) {
        writeRaw (OS, b);
    }
}

template <typename ELFT>
void OutputSection<ELFT>::append (pstore::repo::ticket_member const & TM, SectionPtr SectionData,
                                  SymbolTable<ELFT> & Symbols) {
    using namespace llvm;

    Contributions_.emplace_back (SectionData);

    auto const ObjectSize = SectionData->data ().size ();
    DEBUG (dbgs () << "  generating relocations FROM " << this->getString (TM.name) << '\n');

    auto const DataAlign = SectionData->align ();
    // ELF section alignment is the maximum of the alignment of all its contributions.
    Align_ = std::max (Align_, DataAlign);

    SectionSize_ += alignedBytes (SectionSize_, DataAlign);
    Symbols.insertSymbol (TM.name, this, SectionSize_, ObjectSize, TM.linkage);

    auto const & XFixups = SectionData->xfixups ();
    for (pstore::repo::external_fixup const & Fixup : XFixups) {
        auto const SymbolIndex = Symbols.insertSymbol (Fixup.name);
        DEBUG (dbgs () << "  generating relocation TO '" << this->getString (Fixup.name)
                       << "' symbol index " << SymbolIndex << '\n');

        Elf_Rela Reloc;
        Reloc.setSymbolAndType (SymbolIndex, Fixup.type, IsMips64EL);
        Reloc.r_offset = Fixup.offset + SectionSize_;
        Reloc.r_addend = Fixup.addend;
        Relocations_.push_back (Reloc);
    }

    // FIXME: what about the internal fixups?

    SectionSize_ += ObjectSize;
}

template <typename ELFT>
std::size_t OutputSection<ELFT>::numSections () const {
    return 1U + static_cast<std::size_t> (Relocations_.size () > 0);
}

template <typename ELFT>
template <typename OutputIt>
OutputIt OutputSection<ELFT>::write (llvm::raw_ostream & OS, SectionNameStringTable & SectionNames,
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
    {
        pstore::address const Discriminator = std::get<1> (Id_);
        std::string const SectionName = this->dataSectionName (Attrs->second.Name, Discriminator);
        DEBUG (llvm::dbgs () << "section " << SectionName << " index " << Index_ << '\n');

        Elf_Shdr SH;
        zero (SH);
        SH.sh_name = SectionNames.insert (SectionName);
        SH.sh_type = Attrs->second.Type;
        SH.sh_flags = Attrs->second.Flags | GroupFlag;
        SH.sh_offset = StartPos;
        SH.sh_size = SectionSize_;
        SH.sh_addralign = Align_;
        *(OutShdr++) = SH;
    }

    if (Relocations_.size () > 0) {
        using RelocationType = typename decltype (Relocations_)::value_type;
        writeAlignmentPadding<RelocationType> (OS);
        auto const RelaStartPos = OS.tell ();
        auto const RelocsSize = this->relocationsSize ();
        OS.write (reinterpret_cast<char const *> (Relocations_.data ()), RelocsSize);

        Elf_Shdr RelaSH;
        zero (RelaSH);
        RelaSH.sh_name = SectionNames.insert (this->relocationSectionName (Attrs->second.Name));
        RelaSH.sh_type = llvm::ELF::SHT_RELA;
        RelaSH.sh_flags =
            llvm::ELF::SHF_INFO_LINK | GroupFlag; // sh_info holds index of the target section.
        RelaSH.sh_offset = RelaStartPos;
        RelaSH.sh_size = RelocsSize;
        RelaSH.sh_link = SectionIndices::SymTab;
        RelaSH.sh_info = Index_; // target section
        RelaSH.sh_entsize = sizeof (RelocationType);
        RelaSH.sh_addralign = alignof (RelocationType);
        *(OutShdr++) = RelaSH;
    }
    return OutShdr;
}

template <typename ELFT>
std::string OutputSection<ELFT>::dataSectionName (std::string SectionName,
                                                  pstore::address DiscriminatorName) const {
    if (DiscriminatorName != pstore::address::null ()) {
        SectionName += '.';
        SectionName += this->getString (DiscriminatorName);
    }
    return SectionName;
}

template <typename ELFT>
std::string OutputSection<ELFT>::relocationSectionName (std::string const & BaseName) const {
    if (BaseName[0] == '.') {
        return std::string{".rela"} + BaseName;
    }
    return std::string{".rela."} + BaseName;
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFOUTPUTSECTION_H
