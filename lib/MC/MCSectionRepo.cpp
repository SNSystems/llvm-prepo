//#include <string>
#include <iostream>

#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCSymbol.h"

using namespace llvm;

namespace {
unsigned idx = 0;
}

MCSectionRepo::MCSectionRepo(SectionKind K, MCSymbol *Begin)
      : MCSection(SV_Repo, K, Begin)
      , index_ {++idx} {

StringRef name = Begin == nullptr ? "<none>" : Begin->getName ();
std::cout << "Section name:" << std::string {name} << " index_:" << index_ << '\n';
  }

MCSectionRepo::MCSectionRepo(SectionKind K, MCSymbol *Begin, std::string id,
               DigestType Digest)
      : MCSection(SV_Repo, K, Begin)
      , id_ {std::move (id)}
      , digest_{ std::move(Digest) }
      , index_ {++idx} {
StringRef name = Begin == nullptr ? "<none>" : Begin->getName ();
std::cout << "Section key:" << id_ << " (symbol:" << std::string (name) << " index_:" << index_ << ")\n";
  }


MCSectionRepo::~MCSectionRepo() {
}

