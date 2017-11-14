//===-- R2OELFStringTable.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef REPO2OBJ_ELF_STRING_TABLE_H
#define REPO2OBJ_ELF_STRING_TABLE_H

#include "llvm/Support/raw_ostream.h"
#include "pstore/database.hpp"
#include "pstore/db_archive.hpp"

#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

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
        return getString (Addr);
    }
    std::size_t length (pstore::address Addr) const {
        return getString (Addr).length ();
    }

private:
    std::string getString (pstore::address Addr) const {
        using namespace pstore::serialize;
        archive::database_reader Source (Db_, Addr);
        return read<std::string> (Source);
    }
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

    std::tuple<std::uint64_t, std::uint64_t> write (llvm::raw_ostream & OS) const;

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
std::tuple<std::uint64_t, std::uint64_t>
StringTable<T, Traits>::write (llvm::raw_ostream & OS) const {
    std::uint64_t Start = OS.tell ();
    OS << '\0';
    for (T const & Name : Data_) {
        assert (OS.tell () - Start == this->position (Name));
        OS << Policy_.get (Name) << '\0';
    }
    std::uint64_t End = OS.tell ();
    assert (End >= Start);
    return std::make_tuple(Start, End - Start);
}


using SectionNameStringTable = StringTable<std::string>;

#endif // REPO2OBJ_ELF_STRING_TABLE_H
