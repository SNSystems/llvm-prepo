#include "pstore/index_types.hpp"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

cl::opt<std::string> TicketPath(cl::Positional, cl::desc("<ticket path>"), cl::Optional,
                                cl::init("./a.out"));

} // anonymous namespace

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
  return OS << Digest.to_hex_string();
}

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv);

  ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  outs() << DigestOrError.get() << '\n';
  return EXIT_SUCCESS;
}
