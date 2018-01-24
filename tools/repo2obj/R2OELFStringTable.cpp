//===-- R2OELFStringTable.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "R2OELFStringTable.h"

#include "llvm/Support/Debug.h"

#include "pstore/database.hpp"
#include "pstore/db_archive.hpp"
#include "pstore/serialize/standard_types.hpp"
#include "pstore/sstring_view_archive.hpp"

// stringToSStringView
// ~~~~~~~~~~~~~~~~~~~
SString stringToSStringView(llvm::StringRef Ref) {
  auto const Length = Ref.size();
  // TODO: check for make_shared array support from C++20.
  auto Ptr =
      std::shared_ptr<char>(new char[Length], [](char *P) { delete[] P; });
  std::copy(std::begin(Ref), std::end(Ref), Ptr.get());
  return {std::static_pointer_cast<char const>(Ptr), Length};
}

// getString
// ~~~~~~~~~
SString getString(pstore::database const &Db, pstore::address Addr) {
  using namespace pstore::serialize;
  archive::database_reader Source(Db, Addr);
  return read<SString>(Source);
}

// insert
// ~~~~~~
std::uint64_t StringTable::insert(SString const &Name) {
  typename container::iterator Pos;
  bool DidInsert;
  std::tie(Pos, DidInsert) = Strings_.emplace(Name, 0);
  if (DidInsert) {
    DEBUG_WITH_TYPE("repo2obj",
                    (llvm::dbgs() << "  strtab insert "
                                  << llvm::StringRef{Name.data(), Name.length()}
                                  << " at " << DataSize_ << '\n'));
    Pos->second = DataSize_;
    DataSize_ += Name.length() + 1;
    Data_.push_back(Name);
  }
  return Pos->second;
}

// position
// ~~~~~~~~~
std::uint64_t StringTable::position(SString const &Name) const {
  auto Pos = Strings_.find(Name);
  return Pos != Strings_.end() ? Pos->second : npos;
}

// write
// ~~~~~
std::tuple<std::uint64_t, std::uint64_t>
StringTable::write(llvm::raw_ostream &OS) const {
  std::uint64_t Start = OS.tell();
  OS << '\0';
  for (SString const &Name : Data_) {
    assert(OS.tell() - Start == this->position(Name));
    OS << llvm::StringRef{Name.data(), Name.length()} << '\0';
  }

  std::uint64_t End = OS.tell();
  assert(End >= Start);
  return std::make_tuple(Start, End - Start);
}
// eof:R2OELFStringTable.cpp
