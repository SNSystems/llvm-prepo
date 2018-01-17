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

#include <map>

template <typename ELFT> class OutputSection;

template <typename ELFT> class SymbolTable {
public:
  explicit SymbolTable(StringTable &Strings) : Strings_{Strings} {}
  SymbolTable(SymbolTable const &) = delete;
  SymbolTable &operator=(SymbolTable const &) = delete;

  struct SymbolTarget {
    /// \param Section_  The ELF output section in which the symbol's data
    /// resides.
    /// \param Offset_  The offset within Section which contains the first byte
    /// of the object.
    /// \param Size_  The object's size (in bytes).
    /// \param Linkage_  The symbol's linkage.
    SymbolTarget(OutputSection<ELFT> const *Section_, std::uint64_t Offset_,
                 std::uint64_t Size_, pstore::repo::linkage_type Linkage_)
        : Section{Section_}, Offset{Offset_}, Size{Size_}, Linkage{Linkage_} {
      assert(Section != nullptr);
    }

    OutputSection<ELFT> const *Section;
    std::uint64_t Offset;
    std::uint64_t Size;
    pstore::repo::linkage_type Linkage;
  };

  struct Value {
    Value() {}
    Value(std::uint64_t NameOffset_, llvm::Optional<SymbolTarget> Target_)
        : NameOffset{NameOffset_}, Target{std::move(Target_)} {}

    pstore::repo::linkage_type linkage() const {
      return Target ? Target.getValue().Linkage
                    : pstore::repo::linkage_type::external;
    }

    /// The offset of the name of this symbol in the symbol names string table.
    std::uint64_t NameOffset = 0;
    /// The location addressed by this symbol (if known).
    llvm::Optional<SymbolTarget> Target;
    /// True if this symbol type is ELF::STT_TLS, otherwise is false.
    bool IsTLS = false;

    std::uint64_t Index = llvm::ELF::STN_UNDEF;
  };

  using SymbolMapType = std::map<SString, Value>;

  /// Find a symbol with name equivalent to Name in the symbol table.
  /// \param Name  The symbol name.
  /// \returns A pointer to pre-existing entry for this name in the symbol
  /// table. If not found, return nullptr.
  Value *findSymbol(SString Name);

  /// Creates a definition in the symbol table.
  /// \param Name  The symbol name.
  /// \param Section  The ELF output section in which the symbol's data resides.
  /// \param Offset  The offset within Section which contains the first byte of
  /// the object.
  /// \param Size  The object's size (in bytes).
  /// \param Linkage  The symbol's linkage.
  /// \returns A pointer to the newly created or pre-existing entry for this
  /// name in the symbol table.
  Value *insertSymbol(SString Name, OutputSection<ELFT> const *Section,
                      std::uint64_t Offset, std::uint64_t Size,
                      pstore::repo::linkage_type Linkage);

  /// If not already in the symbol table, an undef entry is created. This may be
  /// later turned into a proper definition by a subsequent call to insertSymbol
  /// with the same name.
  /// \param Name  The symbol name.
  /// \param Type  The symbol relocation type.
  /// \returns A pointer to the newly created or pre-existing entry for this
  /// name in the symbol table.
  Value *insertSymbol(SString Name, pstore::repo::relocation_type Type);

  /// \returns A tuple of two values, the first of which is the file offset at
  /// which the section data was written; the second is the number of bytes that
  /// were written.
  std::tuple<std::uint64_t, std::uint64_t>
  write(llvm::raw_ostream &OS, std::vector<Value *> const &OrderedSymbols);

  /// As a side effect, sets the Index field on the symbol entries to allow the
  /// index of any symbol to be quickly discovered.
  /// \note Don't insert any symbols after calling this function!
  std::vector<Value *> sort();

  static unsigned firstNonLocal(std::vector<Value *> const &OrderedSymbols);

private:
  static unsigned linkageToELFBinding(pstore::repo::linkage_type L);
  static unsigned sectionToSymbolType(ELFSectionType T);
  static bool isTLSRelocation(pstore::repo::relocation_type Type);

  Value *insertSymbol(SString Name, llvm::Optional<SymbolTarget> const &Target);

  typedef typename llvm::object::ELFFile<ELFT>::Elf_Sym Elf_Sym;

  SymbolMapType SymbolMap_;
  StringTable &Strings_;
};

template <typename ELFT>
unsigned SymbolTable<ELFT>::linkageToELFBinding(pstore::repo::linkage_type L) {
  switch (L) {
  case pstore::repo::linkage_type::internal:
    return llvm::ELF::STB_LOCAL;
  case pstore::repo::linkage_type::linkonce:
    return llvm::ELF::STB_WEAK;
  default:
    return llvm::ELF::STB_GLOBAL;
  }
}

template <typename ELFT>
unsigned SymbolTable<ELFT>::sectionToSymbolType(ELFSectionType T) {
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
bool SymbolTable<ELFT>::isTLSRelocation(pstore::repo::relocation_type Type) {
  switch (Type) {
  case llvm::ELF::R_X86_64_DTPMOD64:
  case llvm::ELF::R_X86_64_DTPOFF64:
  case llvm::ELF::R_X86_64_TPOFF64:
  case llvm::ELF::R_X86_64_TLSGD:
  case llvm::ELF::R_X86_64_TLSLD:
  case llvm::ELF::R_X86_64_DTPOFF32:
  case llvm::ELF::R_X86_64_GOTTPOFF:
  case llvm::ELF::R_X86_64_TPOFF32:
    return true;
  default:
    return false;
  }
}

// findSymbol
// ~~~~~~~~~~
template <typename ELFT>
auto SymbolTable<ELFT>::findSymbol(SString Name) -> Value * {
  auto Pos = SymbolMap_.find(Name);
  return Pos != SymbolMap_.end() ? &Pos->second : nullptr;
}

// insertSymbol
// ~~~~~~~~~~~~
template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(SString Name,
                                     OutputSection<ELFT> const *Section,
                                     std::uint64_t Offset, std::uint64_t Size,
                                     pstore::repo::linkage_type Linkage)
    -> Value * {
  auto SV =
      this->insertSymbol(Name, SymbolTarget(Section, Offset, Size, Linkage));
  SV->IsTLS = sectionToSymbolType(Section->getType()) == llvm::ELF::STT_TLS;
  return SV;
}

template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(SString Name,
                                     pstore::repo::relocation_type Type)
    -> Value * {
  auto SV = this->insertSymbol(Name, llvm::None);
  SV->IsTLS = isTLSRelocation(Type);
  return SV;
}

template <typename ELFT>
auto SymbolTable<ELFT>::insertSymbol(SString Name,
                                     llvm::Optional<SymbolTarget> const &Target)
    -> Value * {
  typename decltype(SymbolMap_)::iterator Pos;
  bool DidInsert;
  std::tie(Pos, DidInsert) = SymbolMap_.emplace(Name, Value{});
  if (DidInsert) {
    Pos->second = Value{Strings_.insert(Name), Target};
    return &Pos->second;
  }

  // If we don't have a value associated with this symbol, then use the one we
  // have here.
  Value &V = Pos->second;
  if (!V.Target) {
    // FIXME: if Target && V.Target, we're attempting to make a duplicate
    // definition which wouldn't be right.
    V.Target = Target;
  }
  return &Pos->second;
}

// write
// ~~~~~
template <typename ELFT>
std::tuple<std::uint64_t, std::uint64_t>
SymbolTable<ELFT>::write(llvm::raw_ostream &OS,
                         std::vector<Value *> const &OrderedSymbols) {
  using namespace llvm;
  writeAlignmentPadding<Elf_Sym>(OS);

  uint64_t const StartOffset = OS.tell();

  // Write the reserved zeroth symbol table entry.
  Elf_Sym Symbol;
  zero(Symbol);
  Symbol.st_shndx = ELF::SHN_UNDEF;
  writeRaw(OS, Symbol);

  for (Value const *SV : OrderedSymbols) {
    zero(Symbol);
    Symbol.st_name = SV->NameOffset;

    if (SV->Target) {
      SymbolTarget const &T = SV->Target.getValue();
      Symbol.st_value = T.Offset;
      auto const ST = sectionToSymbolType(T.Section->getType());
      assert(SV->IsTLS == (ST == llvm::ELF::STT_TLS));
      Symbol.setBindingAndType(linkageToELFBinding(T.Linkage), ST);
      // The section (header table index) in which this value is defined.
      Symbol.st_shndx = T.Section->getIndex();
      Symbol.st_size = T.Size;
    } else {
      // There's no definition for this name.
      Symbol.setBindingAndType(ELF::STB_GLOBAL,
                               SV->IsTLS ? ELF::STT_TLS : ELF::STT_NOTYPE);
      Symbol.st_shndx = ELF::SHN_UNDEF;
    }

    writeRaw(OS, Symbol);
  }
  return std::make_tuple(StartOffset, OS.tell() - StartOffset);
}

template <typename ELFT>
auto SymbolTable<ELFT>::sort() -> std::vector<Value *> {
  std::vector<Value *> OrderedSymbols;
  OrderedSymbols.reserve(SymbolMap_.size());
  for (auto &S : SymbolMap_) {
    OrderedSymbols.push_back(&S.second);
  }

  std::sort(std::begin(OrderedSymbols), std::end(OrderedSymbols),
            [](Value const *const A, Value const *const B) {
              return A->linkage() == pstore::repo::linkage_type::internal &&
                     B->linkage() != pstore::repo::linkage_type::internal;
            });

  // Finally tell the symbols about their indices.
  unsigned Index = 0;
  for (auto &S : OrderedSymbols) {
    S->Index = ++Index;
  }
  return OrderedSymbols;
}

template <typename ELFT>
unsigned
SymbolTable<ELFT>::firstNonLocal(std::vector<Value *> const &OrderedSymbols) {
  using Iterator = typename std::vector<Value *>::const_iterator;
  auto Count =
      static_cast<typename std::iterator_traits<Iterator>::difference_type>(
          OrderedSymbols.size());
  auto First = std::begin(OrderedSymbols);

  while (Count > 0U) {
    auto It = First;
    auto const Step = Count / 2;
    std::advance(It, Step);

    if ((*It)->linkage() == pstore::repo::linkage_type::internal) {
      First = ++It;
      Count -= Step + 1;
    } else {
      Count = Step;
    }
  }
  return First != std::end(OrderedSymbols) ? (*First)->Index : 1U;
}

#endif // LLVM_TOOLS_REPO2OBJ_ELFSYMBOLTABLE_H
