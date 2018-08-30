//===- RepoHashCalculator.h - Implement Hash Calculation --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the FunctionHash and  VariableHash Calculator which
// are used as 'ticket' item by the RepoTicketGeneration passes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_REPOHASHCALCULATOR_H
#define LLVM_TRANSFORMS_UTILS_REPOHASHCALCULATOR_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/RepoTicket.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/Support/AtomicOrdering.h"
#include "llvm/Transforms/Utils/FunctionComparator.h"

namespace llvm {

class GetElementPtrInst;

enum HashKind {
  TAG_Uint32,
  TAG_Uint64,
  TAG_StringRef,
  TAG_APInt,
  TAG_APFloat,
  TAG_AtomicOrdering,
  TAG_AttributeEnum,
  TAG_AttributeInt,
  TAG_AttributeString,
  TAG_AttributeList,
  TAG_InlineAsm,
  TAG_InlineAsm_SideEffects,
  TAG_InlineAsm_AlignStack,
  TAG_InlineAsm_Dialect,
  TAG_RangeMetadata,
  TAG_Type,
  TAG_Constant,
  TAG_Value,
  TAG_Signature,
  TAG_Signature_GC,
  TAG_Signature_Sec,
  TAG_Signature_VarArg,
  TAG_Signature_CC,
  TAG_Signature_Arg,
  TAG_OperandBundles,
  TAG_Instruction,
  TAG_GetElementPtrInst,
  TAG_AllocaInst,
  TAG_LoadInst,
  TAG_StoreInst,
  TAG_CmpInst,
  TAG_CallInst,
  TAG_InvokeInst,
  TAG_InsertValueInst,
  TAG_ExtractValueInst,
  TAG_FenceInst,
  TAG_AtomicCmpXchgInst,
  TAG_AtomicRMWInst,
  TAG_PHINode,
  TAG_BasicBlock,
  TAG_GlobalFunction,
  TAG_GlobalVariable,
  TAG_GlobalAlias,
  TAG_GVName,
  TAG_GVIsConstant,
  TAG_GVVisibility,
  TAG_GVThreadLocalMode,
  TAG_GVAlignment,
  TAG_GVUnnamedAddr,
  TAG_GVDLLStorageClassType,
  TAG_Datalayout,
  TAG_Triple,
};

class HashCalculator {
public:
  /// Start the calculation.
  void beginCalculate(const Module &M) {
    SNMap.clear();
    GlobalNumbers.clear();
    reset(M);
  }

  /// Incrementally add the bytes in \p Data to the hash.
  template <typename Ty> void update(ArrayRef<Ty> Data) {
    Hash.update(
        ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Data.data()),
                          Data.size() * sizeof(Ty)));
  }

  MD5::MD5Result getHashResult() {
    MD5::MD5Result Result;
    Hash.final(Result);
    return Result;
  }

  void reset(const Module &M) {
    Hash = MD5();
    assert(M.getModuleHash().hasValue());
    Hash.update(M.getModuleHash().getValue().Bytes);
  }

  template <typename Ty> void hashNumber(Ty V) {
    Hash.update(
        ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(&V), sizeof(V)));
  }

  void hashMem(StringRef V);
  void hashAPInt(const APInt &V);
  void hashAPFloat(const APFloat &V);
  void hashOrdering(AtomicOrdering V);
  void hashAttribute(const Attribute &V);
  void hashAttributeList(const AttributeList &V);
  void hashInlineAsm(const InlineAsm *V);
  void hashRangeMetadata(const MDNode *V);

  /// typeHash - accumulate a type hash,
  ///
  /// 1. If type is one of PrimitiveTypes (different type IDs), use the type ID
  /// to calculate the hash.
  ///    * Void
  ///    * Float
  ///    * Double
  ///    * X86_FP80
  ///    * FP128
  ///    * PPC_FP128
  ///    * Label
  ///    * Metadata
  /// 2. If types are integers, type and the width to calculate the hash.
  /// 3. If they are vectors, use the vector type, count and subtype to
  /// calculate the hash.
  /// 4. If the type is pointer, use pointer address space hash to calculate the
  /// hash.
  /// 5. If types are complex, use type and  element type to calculate the hash.
  /// 6. For all other cases put llvm_unreachable.
  void hashType(Type *Ty);

  /// Constants Hash accumulate.
  /// 1. Accumulate the type ID of V.
  /// 2. Accumulate constant contents.
  void hashConstant(const Constant *V);

  /// Assign or look up previously assigned number for the value. Numbers are
  /// assigned in the order visited.
  void hashValue(const Value *V);

  /// Accumulate th global values by number. Uses the GlobalNumbersState to
  /// identify the same gobals across function calls.
  void hashGlobalValue(const GlobalValue *V);

  /// Return the computed hash as a string.
  std::string &get(MD5::MD5Result &HashRes);

  ticketmd::DependenciesType &getDependencies() { return Dependencies; }

private:
  // Accumulate the hash of basicblocks, instructions and variables etc in the
  // function Fn.
  MD5 Hash;

  // Hold the global object list which the function hash depenendent on.
  ticketmd::DependenciesType Dependencies;

  /// Assign serial numbers to values from the function.
  /// Explanation:
  /// Being caclulating functions we need to compare values.
  /// Its easy to sort things out for external values. It just should be
  /// the same value for all functions.
  /// But for local values (those were introduced inside function body)
  /// we have to ensure they were introduced at exactly the same place,
  /// and plays the same role.
  /// Let's assign serial number to each value when we meet it first time.
  /// Values that were met at same place will be with same serial numbers.
  /// In this case it would be good to explain few points about values assigned
  /// to BBs and other ways of implementation (see below).
  ///
  /// 1. Safety of BB reordering.
  /// It's safe to change the order of BasicBlocks in function.
  /// Relationship with other functions and serial numbering will not be
  /// changed in this case.
  /// As follows from FunctionHashCalculator::calculateHash(), we do CFG walk:
  /// we start from the entry, and then take each terminator. So it doesn't
  /// matter how in fact BBs are ordered in function. And since valueHash are
  /// called during this walk, the numbering depends only on how BBs located
  /// inside the CFG. So the answer is - yes. We will get the same numbering.
  ///
  /// 2. Impossibility to use dominance properties of values.
  /// If we calculate instruction operands: first is usage of local variable
  /// from function, we could calulate the origin and check whether they are
  /// defined at the same place. But, we are still not able to calulate operands
  /// of PHI nodes, since those could be operands from further BBs we didn't
  /// scan yet. So it's impossible to use dominance properties in general.
  DenseMap<const Value *, unsigned> SNMap;

  // The global state we will use.
  DenseMap<const GlobalValue *, unsigned> GlobalNumbers;

  std::string TheHash;
};

/// FunctionHashCalculator - Calculate the function hash.
class FunctionHashCalculator {
public:
  FunctionHashCalculator(const Function *F) : Fn(F) {}

  /// Calculate the hash for the function.
  void calculateHash();

  /// Return the function hash result.
  MD5::MD5Result getHashResult();

  /// Incrementally add the bytes in \p Data to the hash.
  template <typename T> void update(const T &Data) {
    FnHash.update(makeArrayRef(Data));
  }

  /// Return the function hash.
  HashCalculator &hasher() { return FnHash; }

protected:
  /// Return the function for unit test.
  const Function *function() const { return Fn; }

  /// Calculate the hash for the signature and other general attributes of the
  /// function.
  void hashSignature(const Function *Fn);

  /// Accumulate the hash for the basic block.
  void hashBasicBlock(const BasicBlock *BB);

  /// Accumulate the hash for the function Fn.
  void hashFunction();

  void hashOperandBundles(const Instruction *V);

  /// Accumulate the common parts of Call and Invoke instructions.
  template <typename T> void hashCallInvoke(const T *Instruction) {
    FnHash.hashNumber(Instruction->getCallingConv());
    FnHash.hashAttributeList(Instruction->getAttributes());
    hashOperandBundles(Instruction);
    FnHash.hashRangeMetadata(Instruction->getMetadata(LLVMContext::MD_range));
  }

  /// Calculate the Instruction hash.
  ///
  /// Stages:
  /// 1. Operations opcodes. Calculate as a number.
  /// 2. Number of operands.
  /// 3. Operation types. Calculate with typeHash method.
  /// 4. Calculate operation subclass optional data as stream of bytes:
  /// just convert it to integers and call numberHash.
  /// 5. Calculate in operation operand types with typeHash.
  /// 6. Last stage. Calculate operations for some specific attributes.
  /// For example, for Load it would be:
  /// 6.1.Load: volatile (as boolean flag)
  /// 6.2.Load: alignment (as integer numbers)
  /// 6.3.Load: ordering (as underlying enum class value)
  /// 6.4.Load: synch-scope (as integer numbers)
  /// 6.5.Load: range metadata (as integer ranges)
  /// On this stage its better to see the code, since its not more than 10-15
  /// strings for particular instruction, and could change sometimes.
  void hashInstruction(const Instruction *V);

private:
  // The function undergoing calculation.
  const Function *Fn;

  // Hold the function hash value.
  HashCalculator FnHash;
};

/// VariableHashCalculator - Calculate the global variable hash.
class VariableHashCalculator {
public:
  VariableHashCalculator(const GlobalVariable *V) : Gv(V) {}

  /// Calculate the global Variable Gv hash value.
  void calculateHash();

  std::string &get(MD5::MD5Result &HashRes) { return GvHash.get(HashRes); }

  MD5::MD5Result getHashResult() { return GvHash.getHashResult(); }

  /// Incrementally add the bytes in \p Data to the hash.
  template <typename T> void update(const T &Data) {
    GvHash.update(makeArrayRef(Data));
  }

  /// Return the variable hash.
  HashCalculator &hasher() { return GvHash; }

private:
  /// Accumulate the hash for the variable Gv.
  void hashVariable();

  // The Variable undergoing calculation.
  const GlobalVariable *Gv;

  // Hold the Variable hash value.
  HashCalculator GvHash;
};

template <typename T> // primary template
struct DigestCalculator {};

template <> // explicit specialization for T = GlobalVariable
struct DigestCalculator<GlobalVariable> {
  using Calculator = VariableHashCalculator;
};

template <> // explicit specialization for T = Function
struct DigestCalculator<Function> {
  using Calculator = FunctionHashCalculator;
};

template <typename T>
ticketmd::DigestAndDependencies calculateDigestAndDependencies(const T *GO) {
  // Calculate the initial global object hash value and dependent list.
  typename DigestCalculator<T>::Calculator GOHC{GO};
  GOHC.calculateHash();
  return std::make_pair(std::move(GOHC.getHashResult()),
                        std::move(GOHC.hasher().getDependencies()));
}

} // end namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_REPOHASHCALCULATOR_H
