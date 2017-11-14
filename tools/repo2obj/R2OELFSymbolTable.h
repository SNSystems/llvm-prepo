//===-- R2OELFSymbolTable.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef REPO2OBJ_ELF_SYMBOL_TABLE_H
#define REPO2OBJ_ELF_SYMBOL_TABLE_H

#include "llvm/Object/ELF.h"
#include "R2OELFStringTable.h"
#include "WriteHelpers.h"

#include <unordered_map>
#include <vector>

using SymbolNameStringTable = StringTable<pstore::address>;

template <typename ELFT>
class SymbolTable {
public:
    explicit SymbolTable (SymbolNameStringTable & Strings)
            : Strings_{Strings} {}
    SymbolTable (SymbolTable const &) = delete;
    SymbolTable & operator= (SymbolTable const &) = delete;

    struct SymbolTarget {
        SymbolTarget (unsigned SectionIndex_, pstore::repo::section_type SectionType_,
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

    std::uint64_t insertSymbol (pstore::address Name, llvm::Optional<SymbolTarget> const & Target);

    /// Creates a definition in the symbol table.
    std::uint64_t insertSymbol (pstore::address Name, unsigned SectionIndex,
                                pstore::repo::section_type Type, std::uint64_t Offset,
                                pstore::repo::linkage_type Linkage) {
        return this->insertSymbol (Name, SymbolTarget{SectionIndex, Type, Offset, Linkage});
    }

    // If not already in the symbol table, an undef entry is created. This may be later turned into
    // a proper definition.
    std::uint64_t insertSymbol (pstore::address Name) {
        return this->insertSymbol (Name, llvm::None);
    }

    std::tuple<std::uint64_t, std::uint64_t> write (llvm::raw_ostream & OS);

private:
    static unsigned linkageToELFBinding (pstore::repo::linkage_type L);

    typedef typename llvm::object::ELFFile<ELFT>::Elf_Sym Elf_Sym;

    struct Value {
        std::size_t SymbolIndex;
        /// The offset of the name of this symbol in the symbol names string table.
        std::uint64_t NameOffset;
        llvm::Optional<SymbolTarget> Target;
    };
    // FIXME: this pair of fields is replicated for the section table. Refactoring?
    std::vector<Value> Symbols_;
    std::unordered_map<pstore::address, uint64_t> SymbolMap_; // TODO: DenseMap?

    SymbolNameStringTable & Strings_;
};

template <typename ELFT>
unsigned SymbolTable<ELFT>::linkageToELFBinding (pstore::repo::linkage_type L) {
#if 1
    unsigned Binding = llvm::ELF::STB_GLOBAL;
#else
    unsigned Binding;
    switch (T.Linkage) {
    case pstore::repo::linkage_type::external:
    case pstore::repo::linkage_type::common:
    case pstore::repo::linkage_type::linkonce:
        Binding = llvm::ELF::STB_GLOBAL;
        break;
    case pstore::repo::linkage_type::internal:
        Binding = llvm::ELF::STB_LOCAL;
        break;
    }
#endif
    return Binding;
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
        Symbols_.push_back (Value{SymbolIndex, Strings_.insert (Name), Target});
        return SymbolIndex;
    }

    // If we don't have a value associated with this symbol, then use the one we have here.
    // FIXME: if Target && V.Target, we're attemptting to make a duplicate definition which wouldnt
    // be right.
    uint64_t const SymbolIndex = Pos->second;
    Value & V = Symbols_[SymbolIndex];
    if (!V.Target) {
        V.Target = Target;
    }
    return SymbolIndex;
}

template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t> SymbolTable<ELFT>::write (llvm::raw_ostream & OS) {
    using namespace llvm;
    writeAlignmentPadding<Elf_Sym> (OS);

    uint64_t const StartOffset = OS.tell ();

    Elf_Sym Symbol;
    // Write the reserved zeroth symbol table entry.
    zero (Symbol);
    Symbol.st_shndx = ELF::SHN_UNDEF;
    writeRaw (OS, Symbol);

    for (Value const & SV : Symbols_) {
        zero (Symbol);
        Symbol.st_name = SV.NameOffset;

        if (SV.Target) {
            SymbolTarget const & T = SV.Target.getValue ();
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
    uint64_t const EndOffset = OS.tell ();
    return std::make_tuple(StartOffset, EndOffset - StartOffset);
}

#endif // REPO2OBJ_ELF_SYMBOL_TABLE_H
