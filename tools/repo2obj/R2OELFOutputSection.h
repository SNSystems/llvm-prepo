//===-- R2OELFOutputSection.h ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef REPO2OBJ_ELF_OUTPUT_SECTION_H
#define REPO2OBJ_ELF_OUTPUT_SECTION_H

#include "llvm/Object/ELF.h"

#include "pstore_mcrepo/fragment.hpp"
#include "pstore_mcrepo/ticket.hpp"

#include "R2OELFSymbolTable.h"
#include "WriteHelpers.h"

#include <cstdint>
#include <map>
#include <memory>
#include <vector>

using SectionPtr = std::shared_ptr<pstore::repo::section const>;

namespace details {

    struct SectionInfo {
        SectionInfo (char const * N, unsigned T, unsigned F)
                : Name{N}
                , Type{T}
                , Flags{F} {}
        char const * Name;
        unsigned Type;  /* sh_type value */
        unsigned Flags; /* sh_flags value */
    };
    using SectionMap = std::map<pstore::repo::section_type, SectionInfo>;
    extern SectionMap const SectionAttributes;

} // namespace details

/// Defines the set of standard (fixed) sections that we put in the ELF file.
enum SectionIndices { Null, SectionNamesStrTab, SymbolNamesStrTab, SymTab, last };


template <typename ELFT>
class OutputSection {
public:
    explicit OutputSection (pstore::database & Db, pstore::repo::section_type T, unsigned Index)
            : Db_{Db}
            , Type_{T}
            , Index_{Index} {}

    std::uint64_t contributionsSize () const {
        return SectionSize_;
    }
    std::uint64_t relocationsSize () const {
        return Relocations_.size () * sizeof (Elf_Rela);
    }

    void append (pstore::repo::ticket_member const & TM, pstore::repo::section_type SectionType,
                 SectionPtr SectionData, SymbolTable<ELFT> & Symbols);

    /// \tparam OutputIt  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type.
    /// \param OS The binary output stream to which the contents of the output sections are written.
    /// \param SectionNames
    /// \param NumSections
    /// \param OutShdr  An output iterator to which will be written one or more instance of the
    /// Elf_Shdr type as the write function creates sections in the output.
    template <typename OutputIt>
    OutputIt write (llvm::raw_ostream & OS, SectionNameStringTable & SectionNames,
                    unsigned NumSections, OutputIt OutShdr) const;

    pstore::repo::section_type getType () const {
        return Type_;
    }
    unsigned getIndex () const {
        return Index_;
    }

private:
    pstore::database & Db_;
    pstore::repo::section_type const Type_;
    unsigned const Index_; // The section header table index
    // TODO: We have no a priori knowledge of the number of text contributions to this output
    // section. Using some sort of "chunked vector" might be considerably more efficient.
    std::vector<SectionPtr> Contributions_;
    std::uint64_t SectionSize_ = 0;

    typedef typename llvm::object::ELFFile<ELFT>::Elf_Rela Elf_Rela;
    typedef typename llvm::object::ELFFile<ELFT>::Elf_Shdr Elf_Shdr;

    std::vector<Elf_Rela> Relocations_;
};

inline std::string getString (pstore::database & Db, pstore::address Addr) {
    using namespace pstore::serialize;
    archive::database_reader Source (Db, Addr);
    return read<std::string> (Source);
}
constexpr bool IsMips64EL = false;

template <typename ELFT>
void OutputSection<ELFT>::append (pstore::repo::ticket_member const & TM,
                                  pstore::repo::section_type SectionType, SectionPtr SectionData,
                                  SymbolTable<ELFT> & Symbols) {
    using namespace llvm;

    Contributions_.emplace_back (SectionData);
    // FIXME: this assumes that the section represents the fragment's one and only section.
    // When we begin to generate multi-section fragments, this will need to be a little
    // more sophisticated. We'll also need to be able to add non-database-resident names
    // to the symbol table.
    dbgs () << "generating relocations FROM " << getString (Db_, TM.name) << '\n';
    Symbols.insertSymbol (TM.name, typename SymbolTable<ELFT>::SymbolTarget{
                                       Index_, SectionType, SectionSize_, TM.linkage});

    auto const InitialSectionSize = SectionSize_;
    SectionSize_ += SectionData->data ().size (); // TODO: account for alignment

    auto const & XFixups = SectionData->xfixups ();
    Relocations_.reserve (Relocations_.size () + XFixups.size ());
    for (pstore::repo::external_fixup const & Fixup : XFixups) {
        Elf_Rela Reloc;
        auto const SymbolIndex = Symbols.insertSymbol (Fixup.name);
        dbgs () << "generating relocation TO '" << getString (Db_, Fixup.name) << "' symbol index "
                << SymbolIndex << '\n';
        Reloc.setSymbolAndType (SymbolIndex, Fixup.type, IsMips64EL);
        Reloc.r_offset = Fixup.offset + InitialSectionSize;
        Reloc.r_addend = Fixup.addend;
        Relocations_.push_back (Reloc);
    }

    // TODO: the internal fixups.
}

template <typename ELFT>
template <typename OutputIt>
OutputIt OutputSection<ELFT>::write (llvm::raw_ostream & OS, SectionNameStringTable & SectionNames,
                                     unsigned NumSections, OutputIt OutShdr) const {
    auto const StartPos = OS.tell ();
    for (auto const & Contribution : Contributions_) {
        pstore::repo::section::container<std::uint8_t> D = Contribution->data ();
        // TODO: alignment.
        OS.write (reinterpret_cast<char const *> (D.data ()), D.size ());
    }
    assert (OS.tell () - StartPos == SectionSize_);

    auto const & Attrs = details::SectionAttributes.find (Type_);
    assert (Attrs != details::SectionAttributes.end ());
    auto const SectionName = Attrs->second.Name;
    {
        Elf_Shdr SH;
        zero (SH);
        SH.sh_name = SectionNames.insert (SectionName);
        SH.sh_type = Attrs->second.Type;
        SH.sh_flags = Attrs->second.Flags;
        SH.sh_offset = StartPos;
        SH.sh_size = SectionSize_;
        *(OutShdr++) = SH;
        ++NumSections;
    }

    if (Relocations_.size () > 0) {
        auto const RelaStartPos = StartPos + SectionSize_;
        assert (RelaStartPos == OS.tell ());
        auto const RelocsSize = this->relocationsSize ();
        OS.write (reinterpret_cast<char const *> (Relocations_.data ()), RelocsSize);
        {
            Elf_Shdr RelaSH;
            zero (RelaSH);
            RelaSH.sh_name = SectionNames.insert (std::string{".rela."} + SectionName);
            RelaSH.sh_type = llvm::ELF::SHT_RELA;
            RelaSH.sh_flags =
                llvm::ELF::SHF_INFO_LINK; // sh_info holds index of the target section.
            RelaSH.sh_offset = RelaStartPos;
            RelaSH.sh_size = RelocsSize;
            RelaSH.sh_link = SectionIndices::SymTab;
            RelaSH.sh_info = NumSections - 1; // target
            RelaSH.sh_entsize = sizeof (Elf_Rela);
            *(OutShdr++) = RelaSH;
            NumSections++;
        }
    }
    return OutShdr;
}

#endif // REPO2OBJ_ELF_OUTPUT_SECTION_H
