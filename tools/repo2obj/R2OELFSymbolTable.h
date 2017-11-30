//===-- R2OELFSymbolTable.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H

#include "llvm/Object/ELF.h"

#include "R2OELFSectionType.h"
#include "R2OELFStringTable.h"
#include "WriteHelpers.h"

#include <unordered_map>
#include <vector>

using SymbolNameStringTable = StringTable<pstore::address>;

template <typename ELFT>
class OutputSection;

template <typename ELFT>
class SymbolTable {
public:
    explicit SymbolTable (SymbolNameStringTable & Strings)
            : Strings_{Strings} {}
    SymbolTable (SymbolTable const &) = delete;
    SymbolTable & operator= (SymbolTable const &) = delete;

    struct SymbolTarget {
        /// \param Section_  The ELF output section in which the symbol's data resides.
        /// \param Offset_  The offset within Section which contains the first byte of the object.
        /// \param Size_  The object's size (in bytes).
        /// \param Linkage_  The symbol's linkage.
        SymbolTarget (OutputSection<ELFT> const * Section_, std::uint64_t Offset_,
                      std::uint64_t Size_, pstore::repo::linkage_type Linkage_)
                : Section{Section_}
                , Offset{Offset_}
                , Size{Size_}
                , Linkage{Linkage_} {
            assert (Section != nullptr);
        }

        OutputSection<ELFT> const * Section;
        std::uint64_t Offset;
        std::uint64_t Size;
        pstore::repo::linkage_type Linkage;
    };


    /// Creates a definition in the symbol table.
    /// \param Name  The symbol name.
    /// \param Section  The ELF output section in which the symbol's data resides.
    /// \param Offset  The offset within Section which contains the first byte of the object.
    /// \param Size  The object's size (in bytes).
    /// \param Linkage  The symbol's linkage.
    /// \returns The index of the newly created or pre-existing entry for this name in the symbol
    /// table.
    std::uint64_t insertSymbol (pstore::address Name, OutputSection<ELFT> const * Section,
                                std::uint64_t Offset, std::uint64_t Size,
                                pstore::repo::linkage_type Linkage) {
        return this->insertSymbol (Name, SymbolTarget (Section, Offset, Size, Linkage));
    }

    /// If not already in the symbol table, an undef entry is created. This may be later turned into
    /// a proper definition by a subsequent call to insertSymbol with the same name.
    /// \param Name  The symbol name.
    /// \returns The index of the newly created or pre-existing entry for this name in the symbol
    /// table.
    std::uint64_t insertSymbol (pstore::address Name) {
        return this->insertSymbol (Name, llvm::None);
    }

    /// \returns A tuple of two values, the first of which is the file offset at which the section
    /// data was written; the second is the number of bytes that were written.
    std::tuple<std::uint64_t, std::uint64_t> write (llvm::raw_ostream & OS);

private:
    static unsigned linkageToELFBinding (pstore::repo::linkage_type L);
    static unsigned sectionToSymbolType (ELFSectionType T);
    std::uint64_t insertSymbol (pstore::address Name, llvm::Optional<SymbolTarget> const & Target);

    typedef typename llvm::object::ELFFile<ELFT>::Elf_Sym Elf_Sym;

    struct Value {
        /// The offset of the name of this symbol in the symbol names string table.
        std::uint64_t NameOffset;
        llvm::Optional<SymbolTarget> Target;
    };

    // FIXME: Vector is rather likely to be inefficient for the symbol table. Refactoring?
    std::vector<Value> Symbols_;
    std::unordered_map<pstore::address, uint64_t> SymbolMap_; // TODO: DenseMap?

    SymbolNameStringTable & Strings_;
};

template <typename ELFT>
unsigned SymbolTable<ELFT>::linkageToELFBinding (pstore::repo::linkage_type L) {
// FIXME: a temporary bodge. We don't sort the symbol table and don't correctly set the sh_
#if 0
    switch (L) {
    case pstore::repo::linkage_type::external:
    case pstore::repo::linkage_type::common:
    case pstore::repo::linkage_type::linkonce:
        return llvm::ELF::STB_GLOBAL;
    case pstore::repo::linkage_type::internal:
        return llvm::ELF::STB_LOCAL;
    }
#endif
    return llvm::ELF::STB_GLOBAL;
}

template <typename ELFT>
unsigned SymbolTable<ELFT>::sectionToSymbolType (ELFSectionType T) {
    using namespace pstore::repo;
    switch (T) {
    case ELFSectionType::Common:
        return llvm::ELF::STT_COMMON;
    case ELFSectionType::Text:
        return llvm::ELF::STT_FUNC;
    case ELFSectionType::BSS:
    case ELFSectionType::Data:
    case ELFSectionType::InitArray:
    case ELFSectionType::FiniArray:
    case ELFSectionType::RelRo:
    case ELFSectionType::Mergeable1ByteCString:
    case ELFSectionType::Mergeable2ByteCString:
    case ELFSectionType::Mergeable4ByteCString:
    case ELFSectionType::MergeableConst4:
    case ELFSectionType::MergeableConst8:
    case ELFSectionType::MergeableConst16:
    case ELFSectionType::MergeableConst32:
    case ELFSectionType::ReadOnly:
        return llvm::ELF::STT_OBJECT;
    case ELFSectionType::ThreadBSS:
    case ELFSectionType::ThreadData:
        return llvm::ELF::STT_TLS;
    default:
        return llvm::ELF::STT_NOTYPE;
    }
}

template <typename ELFT>
std::uint64_t SymbolTable<ELFT>::insertSymbol (pstore::address Name,
                                               llvm::Optional<SymbolTarget> const & Target) {
    typename decltype (SymbolMap_)::iterator Pos;
    bool DidInsert;
    std::tie (Pos, DidInsert) = SymbolMap_.emplace (Name, 0);
    if (DidInsert) {
        uint64_t const SymbolIndex = SymbolMap_.size ();
        Pos->second = SymbolIndex;
        Symbols_.push_back (Value{Strings_.insert (Name), Target});
        return SymbolIndex;
    }

    // If we don't have a value associated with this symbol, then use the one we have here.
    // Note that SymbolIndex is the index of the symbol in the ELF symbol table. The Symbols_ array
    // does not include the null symbol so we have to subtract 1.
    uint64_t const SymbolIndex = Pos->second;
    assert (SymbolIndex > 0 && SymbolIndex <= Symbols_.size ());
    Value & V = Symbols_[SymbolIndex - 1];
    if (!V.Target) {
        // FIXME: if Target && V.Target, we're attemptting to make a duplicate definition which
        // wouldn't be right.
        V.Target = Target;
    }
    return SymbolIndex;
}

template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t> SymbolTable<ELFT>::write (llvm::raw_ostream & OS) {
    using namespace llvm;
    writeAlignmentPadding<Elf_Sym> (OS);

    uint64_t const StartOffset = OS.tell ();

    // Write the reserved zeroth symbol table entry.
    Elf_Sym Symbol;
    zero (Symbol);
    Symbol.st_shndx = ELF::SHN_UNDEF;
    writeRaw (OS, Symbol);

    for (Value const & SV : Symbols_) {
        zero (Symbol);
        Symbol.st_name = SV.NameOffset;

        if (SV.Target) {
            SymbolTarget const & T = SV.Target.getValue ();
            Symbol.st_value = T.Offset;
            Symbol.setBindingAndType (linkageToELFBinding (T.Linkage),
                                      sectionToSymbolType (T.Section->getType ()));
            // The section (header table index) in which this value is defined.
            Symbol.st_shndx = T.Section->getIndex ();
            Symbol.st_size = T.Size;
        } else {
            // There's no definition for this name.
            Symbol.setBindingAndType (ELF::STB_GLOBAL, ELF::STT_NOTYPE);
            Symbol.st_shndx = ELF::SHN_UNDEF;
        }

        writeRaw (OS, Symbol);
    }
    return std::make_tuple (StartOffset, OS.tell () - StartOffset);
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H
