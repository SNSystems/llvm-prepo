//===-- RepoTicket.h - Program repository digest data structure. -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the digest data structure.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_IR_REPO_TICKET_H
#define LLVM_IR_REPO_TICKET_H

#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/MD5.h"
#include <map>

namespace llvm {
/// Global value digest description.
struct Digest {
  using DigestType = MD5::MD5Result;
  static constexpr size_t DigestSize = 16;
  using GlobalValueMap = std::map<const GlobalValue *, const DigestType>;
  static const Constant *getAliasee(const GlobalAlias *GA);
  // Create a hash file for debugging purpose.
  static void createDigestFile(Module const &M, GlobalValueMap const &ObjMap,
                               StringRef FileExt);
  /// \brief Get global object digest metadata value.
  static void set(Module const &M, GlobalObject *GO, DigestType const &D);
  /// \brief Get global object digest metadata value.
  static DigestType get(const GlobalObject *GO);
};

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
             GlobalValue::LinkageTypes Linkage, bool IsComdat,
             ArrayRef<Metadata *> MDs)
      : MDNode(C, TicketNodeKind, Storage, MDs) {
    assert(MDs.size() == 2 && "Expected a hash and name.");
	static_assert(CheckLinkageType::isSafeCast (), "Linkage type will overflow!");
    SubclassData32 = static_cast<unsigned>(Linkage);
    SubclassData16 = static_cast<unsigned short>(IsComdat);
  }
  ~TicketNode() { dropAllReferences(); }

  static TicketNode *getImpl(LLVMContext &Context, MDString *Name,
                             ConstantAsMetadata *GVHash,
                             GlobalValue::LinkageTypes Linkage, bool IsComdat,
                             StorageType Storage, bool ShouldCreate = true);

  static TicketNode *getImpl(LLVMContext &Context, StringRef Name,
                             Digest::DigestType const &Digest,
                             GlobalValue::LinkageTypes Linkage, bool IsComdat,
                             StorageType Storage, bool ShouldCreate = true);

  TempTicketNode cloneImpl() const {
    // Get the raw name/hash since it is possible to invoke this on
    // a TicketNode containing temporary metadata.
    return getTemporary(getContext(), getNameAsString(), getDigest(),
                        getLinkage(), isComdat());
  }

public:
  static TicketNode *get(LLVMContext &Context, MDString *Name,
                         ConstantAsMetadata *GVHash,
                         GlobalValue::LinkageTypes Linkage, bool IsComdat) {
    return getImpl(Context, Name, GVHash, Linkage, IsComdat, Uniqued);
  }

  static TicketNode *get(LLVMContext &Context, StringRef Name,
                         Digest::DigestType const &Digest,
                         GlobalValue::LinkageTypes Linkage, bool IsComdat) {
    return getImpl(Context, Name, Digest, Linkage, IsComdat, Uniqued);
  }

  static TicketNode *getIfExists(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool IsComdat) {
    return getImpl(Context, Name, GVHash, Linkage, IsComdat, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getIfExists(LLVMContext &Context, StringRef Name,
                                 Digest::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool IsComdat) {
    return getImpl(Context, Name, Digest, Linkage, IsComdat, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getDistinct(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool IsComdat) {
    return getImpl(Context, Name, GVHash, Linkage, IsComdat, Distinct);
  }

  static TicketNode *getDistinct(LLVMContext &Context, StringRef Name,
                                 Digest::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 bool IsComdat) {
    return getImpl(Context, Name, Digest, Linkage, IsComdat, Distinct);
  }

  static TempTicketNode getTemporary(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     GlobalValue::LinkageTypes Linkage,
                                     bool IsComdat) {
    return TempTicketNode(
        getImpl(Context, Name, GVHash, Linkage, IsComdat, Temporary));
  }

  static TempTicketNode getTemporary(LLVMContext &Context, StringRef Name,
                                     Digest::DigestType const &Digest,
                                     GlobalValue::LinkageTypes Linkage,
                                     bool IsComdat) {
    return TempTicketNode(
        getImpl(Context, Name, Digest, Linkage, IsComdat, Temporary));
  }

  /// Return a (temporary) clone of this.
  TempTicketNode clone() const { return cloneImpl(); }

  GlobalValue::LinkageTypes getLinkage() const {
    return static_cast<GlobalValue::LinkageTypes>(SubclassData32);
  }
  bool isComdat() const { return static_cast<bool>(SubclassData16); }

  Metadata *getNameAsMD() const { return getOperand(0); }
  MDString *getNameAsMDString() const { return cast<MDString>(getNameAsMD()); }
  StringRef getNameAsString() const { return getNameAsMDString()->getString(); }

  Metadata *getDigestAsMD() const { return getOperand(1); }
  ConstantAsMetadata *getDigestAsMDConstant() const {
    return cast<ConstantAsMetadata>(getDigestAsMD());
  }
  Digest::DigestType getDigest() const;

  static bool classof(const Metadata *MD) {
    return MD->getMetadataID() == TicketNodeKind;
  }
};

} // end namespace llvm

#endif // LLVM_IR_REPO_TICKET_H
