//===-- Digest.h - Program repository digest data structure. ---*- C++ -*-===//
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

#ifndef LLVM_IR_DIGEST_H
#define LLVM_IR_DIGEST_H

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
public:
  static constexpr size_t MaxLinkageType =
      GlobalValue::LinkageTypes::CommonLinkage;

private:
  friend class LLVMContextImpl;
  friend class MDNode;

  TicketNode(LLVMContext &C, StorageType Storage, unsigned Linkage,
             ArrayRef<Metadata *> MDs)
      : MDNode(C, TicketNodeKind, Storage, MDs) {
    assert(MDs.size() == 2 && "Expected a hash and linkage type.");
    SubclassData16 = Linkage;
  }
  ~TicketNode() { dropAllReferences(); }

  static TicketNode *getImpl(LLVMContext &Context, MDString *Name,
                             ConstantAsMetadata *GVHash, unsigned Linkage,
                             StorageType Storage, bool ShouldCreate = true);

  static TicketNode *getImpl(LLVMContext &Context, StringRef Name,
                             Digest::DigestType const &Digest, unsigned Linkage,
                             StorageType Storage, bool ShouldCreate = true);

  TempTicketNode cloneImpl() const {
    // Get the raw name/hash since it is possible to invoke this on
    // a TicketNode containing temporary metadata.
    return getTemporary(getContext(), getNameAsString(), getDigest(),
                        getLinkage());
  }

public:
  static TicketNode *get(LLVMContext &Context, MDString *Name,
                         ConstantAsMetadata *GVHash, unsigned Linkage) {
    return getImpl(Context, Name, GVHash, Linkage, Uniqued);
  }

  static TicketNode *get(LLVMContext &Context, StringRef Name,
                         Digest::DigestType const &Digest, unsigned Linkage) {
    return getImpl(Context, Name, Digest, Linkage, Uniqued);
  }

  static TicketNode *getIfExists(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash, unsigned Linkage) {
    return getImpl(Context, Name, GVHash, Linkage, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getIfExists(LLVMContext &Context, StringRef Name,
                                 Digest::DigestType const &Digest,
                                 unsigned Linkage) {
    return getImpl(Context, Name, Digest, Linkage, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getDistinct(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash, unsigned Linkage) {
    return getImpl(Context, Name, GVHash, Linkage, Distinct);
  }

  static TicketNode *getDistinct(LLVMContext &Context, StringRef Name,
                                 Digest::DigestType const &Digest,
                                 unsigned Linkage) {
    return getImpl(Context, Name, Digest, Linkage, Distinct);
  }

  static TempTicketNode getTemporary(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     unsigned Linkage) {
    return TempTicketNode(getImpl(Context, Name, GVHash, Linkage, Temporary));
  }

  static TempTicketNode getTemporary(LLVMContext &Context, StringRef Name,
                                     Digest::DigestType const &Digest,
                                     unsigned Linkage) {
    return TempTicketNode(getImpl(Context, Name, Digest, Linkage, Temporary));
  }

  /// Return a (temporary) clone of this.
  TempTicketNode clone() const { return cloneImpl(); }

  unsigned getLinkage() const { return SubclassData16; }
  GlobalValue::LinkageTypes getLinkageType() const {
    return static_cast<GlobalValue::LinkageTypes>(SubclassData16);
  }

  bool isValidLinkage() const {
    GlobalValue::LinkageTypes Type = getLinkageType();
    return GlobalValue::isExternalLinkage(Type) ||
           GlobalValue::isLocalLinkage(Type) ||
           GlobalValue::isWeakLinkage(Type) ||
           GlobalValue::isLinkOnceLinkage(Type) ||
           GlobalValue::isAvailableExternallyLinkage(Type) ||
           GlobalValue::isAppendingLinkage(Type) ||
           GlobalValue::isExternalWeakLinkage(Type) ||
           GlobalValue::isCommonLinkage(Type);
  }

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

#endif // LLVM_IR_DIGEST_H
