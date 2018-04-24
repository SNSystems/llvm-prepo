//===- RepoTicket.h - Program repository digest data structure. -*- C++ --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines the digest data structure.
//
//===---------------------------------------------------------------------===//

#ifndef LLVM_IR_REPO_TICKET_H
#define LLVM_IR_REPO_TICKET_H

#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/MD5.h"

#include <map>
#include <tuple>

namespace llvm {

struct Ticket;

/// Global value ticket metadata description.
namespace ticketmd {
using DigestType = MD5::MD5Result;
static constexpr size_t DigestSize =
    std::tuple_size<decltype(DigestType::Bytes)>::value;
using DependenciesType = SmallVector<const GlobalObject *, 1>;
using DigestAndDependencies = std::pair<DigestType, DependenciesType>;

const Constant *getAliasee(const GlobalAlias *GA);
/// Set global object ticket metadata value and add the metadata to
/// the module level metadta named repo.tickets.
void set(GlobalObject *GO, DigestType const &D);
/// Get global object digest metadata value. Create the Metadat if it does not
/// exist.
std::pair<DigestType, bool> get(const GlobalObject *GO);

/// A structure of a global object (GO) information.
struct GOInfo {
  /// GO's initial hash value which does not include the hash of its dependents.
  DigestType InitialDigest;
  /// GO's dependent global objects.
  DependenciesType Dependencies;

  GOInfo(DigestType &&Digest, DependenciesType &&Dependencies)
      : InitialDigest(std::move(Digest)),
        Dependencies(std::move(Dependencies)) {}
};
using GOInfoMap = DenseMap<GlobalObject *, GOInfo>;

/// A tuple containing the global object information and two unsigned values
/// which are the number of global variables and functions respectively.
using ModuleTuple = std::tuple<GOInfoMap, unsigned, unsigned>;

/// Calculate the initial hash value and dependent lists for all module global
/// objects. The value does not include the hash value of its dependent globals.
/// \param M Called module.
/// \returns a tuple containing the global object information and two
/// unsigned values which are the number of global variables and functions
/// respectively.
ModuleTuple calculateInitialDigestAndDependencies(Module &M);

/// Compute the hash value and set the ticket metadata for all global objects
/// inside of the Module M.
/// \param M Called module.
/// \returns a tuple containing a global object information map which include if
/// the module M has been changed, and two unsigned values which are the number
/// of global variables and functions respectively.
std::tuple<bool, unsigned, unsigned> generateTicketMDs(Module &M);

} // namespace ticketmd

/// Global value ticket node description.
class TicketNode : public MDNode {
  friend class LLVMContextImpl;
  friend class MDNode;

  struct CheckLinkageType {
    using LT = std::underlying_type<GlobalValue::LinkageTypes>::type;
    using ULT = std::make_unsigned<LT>::type;
    template <typename U> static constexpr U max() {
      return std::numeric_limits<U>::max();
    }
    static constexpr bool isSafeCast() {
      return (std::is_unsigned<LT>::value ? true : max<LT>() > 0) &&
             max<ULT>() <= max<unsigned>();
    }
  };

  TicketNode(LLVMContext &C, StorageType Storage,
             GlobalValue::LinkageTypes Linkage, bool Pruned,
             ArrayRef<Metadata *> MDs)
      : MDNode(C, TicketNodeKind, Storage, MDs) {
    assert(MDs.size() == 2 && "Expected a hash and name.");
    static_assert(CheckLinkageType::isSafeCast(),
                  "Linkage type will overflow!");
    SubclassData32 = static_cast<unsigned>(Linkage);
    SubclassData16 = static_cast<unsigned short>(Pruned);
  }
  ~TicketNode() { dropAllReferences(); }

  static TicketNode *getImpl(LLVMContext &Context, MDString *Name,
                             ConstantAsMetadata *GVHash,
                             GlobalValue::LinkageTypes Linkage, bool Pruned,
                             StorageType Storage, bool ShouldCreate = true);

  static TicketNode *getImpl(LLVMContext &Context, StringRef Name,
                             ticketmd::DigestType const &Digest,
                             GlobalValue::LinkageTypes Linkage, bool Pruned,
                             StorageType Storage, bool ShouldCreate = true);

  TempTicketNode cloneImpl() const {
    // Get the raw name/hash since it is possible to invoke this on
    // a TicketNode containing temporary metadata.
    return getTemporary(getContext(), getNameAsString(), getDigest(),
                        getLinkage(), getPruned());
  }

public:
  static TicketNode *get(LLVMContext &Context, MDString *Name,
                         ConstantAsMetadata *GVHash,
                         GlobalValue::LinkageTypes Linkage, bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Pruned, Uniqued);
  }

  static TicketNode *get(LLVMContext &Context, StringRef Name,
                         ticketmd::DigestType const &Digest,
                         GlobalValue::LinkageTypes Linkage, bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Pruned, Uniqued);
  }

  static TicketNode *getIfExists(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getIfExists(LLVMContext &Context, StringRef Name,
                                 ticketmd::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getDistinct(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Pruned, Distinct);
  }

  static TicketNode *getDistinct(LLVMContext &Context, StringRef Name,
                                 ticketmd::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Pruned, Distinct);
  }

  static TempTicketNode getTemporary(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     GlobalValue::LinkageTypes Linkage,
                                     bool Pruned) {
    return TempTicketNode(
        getImpl(Context, Name, GVHash, Linkage, Pruned, Temporary));
  }

  static TempTicketNode getTemporary(LLVMContext &Context, StringRef Name,
                                     ticketmd::DigestType const &Digest,
                                     GlobalValue::LinkageTypes Linkage,
                                     bool Pruned) {
    return TempTicketNode(
        getImpl(Context, Name, Digest, Linkage, Pruned, Temporary));
  }

  /// Return a (temporary) clone of this.
  TempTicketNode clone() const { return cloneImpl(); }

  GlobalValue::LinkageTypes getLinkage() const {
    return static_cast<GlobalValue::LinkageTypes>(SubclassData32);
  }

  bool getPruned() const { return static_cast<bool>(SubclassData16); }
  void setPruned(bool Value) {
    SubclassData16 = static_cast<unsigned short>(Value);
  }

  Metadata *getNameAsMD() const { return getOperand(0); }
  MDString *getNameAsMDString() const { return cast<MDString>(getNameAsMD()); }
  StringRef getNameAsString() const { return getNameAsMDString()->getString(); }

  Metadata *getDigestAsMD() const { return getOperand(1); }
  ConstantAsMetadata *getDigestAsMDConstant() const {
    return cast<ConstantAsMetadata>(getDigestAsMD());
  }
  ticketmd::DigestType getDigest() const;

  static bool classof(const Metadata *MD) {
    return MD->getMetadataID() == TicketNodeKind;
  }
};

} // end namespace llvm

#endif // LLVM_IR_REPO_TICKET_H
