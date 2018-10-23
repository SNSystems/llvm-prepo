//===- MCRepoTicketFile.h - Repo Ticket File --------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOTICKETFILE_H
#define LLVM_MC_MCREPOTICKETFILE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorOr.h"
#include <system_error>

namespace pstore {
class uint128;
namespace index {
using digest = uint128;
} // end namespace index
} // end namespace pstore

namespace llvm {
class raw_ostream;
class MemoryBufferRef;

namespace repo {

extern const uint64_t TicketFileSize;

const std::error_category &ticketErrorCategory();
enum class TicketError { CorruptedTicket = 1 };
inline std::error_code make_error_code(TicketError E) {
  return {static_cast<int>(E), ticketErrorCategory()};
}

void writeTicketFile(support::endian::Writer &W,
                     pstore::index::digest const &Digest);

ErrorOr<pstore::index::digest> getTicketId(llvm::MemoryBufferRef const & Buffer);
ErrorOr<pstore::index::digest> getTicketIdFromFile(StringRef TicketPath);

} // end namespace repo
} // end namespace llvm

namespace std {

template <>
struct is_error_code_enum<llvm::repo::TicketError> : std::true_type {};

} // end namespace std

#endif // LLVM_MC_MCREPOTICKETFILE_H
