//===-- R2OELFStringTable.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H

#include "llvm/Support/raw_ostream.h"
#include "pstore/sstring_view.hpp"

#include <list>
#include <string>
#include <tuple>
#include <unordered_map>

namespace pstore {
    class database;
    struct address;
} // namespace pstore

using SString = ::pstore::sstring_view<std::shared_ptr<char const>>;

SString stringToSStringView (std::string const & Str);
SString getString (pstore::database const & Db, pstore::address Addr);

namespace llvm {
    inline raw_ostream & operator<< (raw_ostream & OS, SString const & S) {
        return OS << StringRef{S.data (), S.length ()};
    }
}

class StringTable {
private:
    using container = std::unordered_map<SString, std::uint64_t>;

public:
    using value_type = typename container::value_type;
    using iterator = typename container::const_iterator;
    using const_iterator = iterator;

    static constexpr auto npos = std::numeric_limits<std::uint64_t>::max ();

    const_iterator begin () const {
        return std::begin (Strings_);
    }
    const_iterator end () const {
        return std::end (Strings_);
    }

    /// Inserts a name into the string table if not already present and returns the string table
    /// offset for the string.
    std::uint64_t insert (SString const & Name);

    /// Writes the string data to the output stream \p OS a returns the extent (position and size)
    /// of the data written.
    std::tuple<std::uint64_t, std::uint64_t> write (llvm::raw_ostream & OS) const;

    std::uint64_t dataSize () const {
        return DataSize_;
    }

private:
    std::uint64_t position (SString const & Name) const;

    container Strings_;
    std::list<SString> Data_;
    std::uint64_t DataSize_ = 1U;
};

#endif // LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H
