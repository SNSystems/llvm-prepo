#include "llvm/MC/MCRepoFragment/MCRepoFragment.h"
#include "llvm/Support/Format.h"

using namespace llvm::repo;

//*  ___         _   _           *
//* / __| ___ __| |_(_)___ _ _   *
//* \__ \/ -_) _|  _| / _ \ ' \  *
//* |___/\___\__|\__|_\___/_||_| *
//*                              *
// sizeBytes
// ~~~~~~~~~
std::size_t Section::sizeBytes(std::size_t DataSize, std::size_t NumIfixups,
                               std::size_t NumXfixups) {
  auto Result = sizeof(Section);
  Result = Section::partSizeBytes<std::uint8_t>(Result, DataSize);
  Result = Section::partSizeBytes<InternalFixup>(Result, NumIfixups);
  Result = Section::partSizeBytes<ExternalFixup>(Result, NumXfixups);
  return Result;
}

std::size_t Section::sizeBytes() const {
  return Section::sizeBytes(data().size(), ifixups().size(), xfixups().size());
}

//*  ___                            _    *
//* | __| _ __ _ __ _ _ __  ___ _ _| |_  *
//* | _| '_/ _` / _` | '  \/ -_) ' \  _| *
//* |_||_| \__,_\__, |_|_|_\___|_||_\__| *
//*             |___/                    *
// Deleter::operator()
// ~~~~~~~~~~~~~~~~~~~
void Fragment::Deleter::operator()(void *P) {
  auto Bytes = reinterpret_cast<std::uint8_t *>(P);
  delete[] Bytes;
}

// operator[]
// ~~~~~~~~~~
Section const &Fragment::operator[](SectionType Key) const {
  auto Offset = Arr_[static_cast<std::size_t>(Key)];
  auto Ptr = reinterpret_cast<std::uint8_t const *>(this) + Offset;
  assert(reinterpret_cast<std::uintptr_t>(Ptr) % alignof(Section) == 0);
  return *reinterpret_cast<Section const *>(Ptr);
}

// operator<<
// ~~~~~~~~~~
namespace llvm {
namespace repo {

#define X(a) case (SectionType::a) : Name=#a ; break;
raw_ostream &operator<<(raw_ostream &OS, SectionType T) {
  char const *Name = "*unknown*";
  switch (T) {
  LLVM_REPO_SECTION_TYPES
  }
  return OS << Name;
}
#undef X

raw_ostream &operator<<(raw_ostream &OS, InternalFixup const &Ifx) {
  using TypeT = std::common_type <unsigned, decltype (Ifx.Type)>::type;
  return OS << "{section:" << static_cast<unsigned>(Ifx.Section)
            << ",type:" << static_cast<TypeT>(Ifx.Type)
            << ",offset:" << Ifx.Offset << ",addend:" << Ifx.Addend << "}";
}

raw_ostream &operator<<(raw_ostream &OS, ExternalFixup const &Xfx) {
  using TypeT = std::common_type <unsigned, decltype (Xfx.Type)>::type;
  return OS << R"({name:")" << Xfx.Name << R"(",type:)" << static_cast <TypeT> (Xfx.Type)
            << ",offset:" << Xfx.Offset << ",addend:" << Xfx.Addend << "}";
}

raw_ostream &operator<<(raw_ostream &OS, Section const &Scn) {
  char const *Indent = "\n  ";
  OS << '{' << Indent << "data: ";
  for (uint8_t V : Scn.data()) {
    OS << format_hex(V, 4U/*width*/) << ',';
  }
  OS << Indent << "ifixups: [ ";
  for (auto const &Ifixup : Scn.ifixups()) {
    OS << Ifixup << ", ";
  }
  OS << ']' << Indent << "xfixups: [ ";
  for (auto const &Xfixup : Scn.xfixups()) {
    OS << Xfixup << ", ";
  }
  return OS << "]\n}";
}

raw_ostream &operator<<(raw_ostream &OS, Fragment const &Frag) {
  for (auto const Key : Frag.sections().getIndices()) {
    auto const Type = static_cast<SectionType>(Key);
    OS << Type << ": " << Frag[Type] << '\n';
  }
  return OS;
}

} // end namespace repo
} // end namespace llvm

// eof:fragment.cpp
