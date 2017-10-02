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
// are used as 'digest' item by the ProgramRepository passes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_REPO_HASH_CALCULATOR_H
#define LLVM_TRANSFORMS_UTILS_REPO_HASH_CALCULATOR_H

#include "llvm/IR/Function.h"
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
  TAG_GVConstant,
  TAG_GVVisibility,
  TAG_GVThreadLocalMode,
  TAG_GVAlignment,
  TAG_GVUnnamedAddr,
  TAG_GVDLLStorageClassType,
  TAG_GVInitValue,
  TAG_Datalayout,
  TAG_Triple,
};

class HashCalculator {
public:
  HashCalculator() {}

  virtual ~HashCalculator() = default;

  /// Start the calculation.
  void beginCalculate() {
    sn_map.clear();
    global_numbers.clear();
    reset();
  }

  /// Incrementally add the bytes in \p Data to the hash.
  void update(ArrayRef<uint8_t> Data) { Hash.update(Data); }

  MD5::MD5Result getHashResult() {
    MD5::MD5Result Result;
    Hash.final(Result);
    return Result;
  }

  void reset() { Hash = MD5(); }

  template <typename Ty>
  void numberHash (Ty V) {
    Hash.update(ArrayRef<uint8_t>((uint8_t *)&V, sizeof(V)));
  }

  void memHash(StringRef V);
  void APIntHash(const APInt &V);
  void APFloatHash(const APFloat &V);
  void orderingHash(AtomicOrdering V);
  void attributeHash(const Attribute &V);
  void attributeListHash(const AttributeList &V);
  void inlineAsmHash(const InlineAsm *V);
  void rangeMetadataHash(const MDNode *V);

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
  void typeHash(Type *Ty);

  /// Constants Hash accumulate.
  /// 1. Accumulate the type ID of V.
  /// 2. Accumulate constant contents.
  void constantHash(const Constant *V);

  /// Assign or look up previously assigned number for the value. Numbers are
  /// assigned in the order visited.
  void valueHash(const Value *V);

  /// Accumulate th global values by number. Uses the GlobalNumbersState to
  /// identify the same gobals across function calls.
  void globalValueHash(const GlobalValue *V);

  // Accumulate the hash of basicblocks, instructions and variables etc in the
  // function Fn.
  // HashAccumulator64 H;
  MD5 Hash;

  /// Return the computed hash as a string.
  std::string &get(MD5::MD5Result &HashRes);

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
  DenseMap<const Value *, unsigned> sn_map;

  // The global state we will use.
  DenseMap<const GlobalValue *, unsigned> global_numbers;

private:
  std::string TheHash;
};

/// FunctionHashCalculator - Calculate the function hash.
class FunctionHashCalculator {
public:
  FunctionHashCalculator(const Function *F) : Fn(F) {}

  /// Calculate the hash for the function.
  void calculateHash(Module &M);

  /// Return the function hash result.
  MD5::MD5Result getHashResult();

  /// Incrementally add the bytes in \p Data to the hash.
  void update(ArrayRef<uint8_t> Data) { FnHash.update(Data); }

protected:
  /// Calculate the hash for the signature and other general attributes of the
  /// function.
  void signatureHash(const Function *Fn);

  /// Accumulate the hash for the target datalayout and triple.
  void moduleHash(Module &M);

  /// Accumulate the hash for the basic block.
  void basicBlockHash(const BasicBlock *BB);

  void operandBundlesHash(const Instruction *V);

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
  void instructionHash(const Instruction *V);

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
  void calculateHash(Module &M);

  std::string &get(MD5::MD5Result &HashRes) { return GvHash.get(HashRes); }

  MD5::MD5Result getHashResult() { return GvHash.getHashResult(); }

  /// Incrementally add the bytes in \p Data to the hash.
  void update(ArrayRef<uint8_t> Data) { GvHash.update(Data); }

private:
  /// Accumulate the hash for the target datalayout and triple.
  void moduleHash(Module &M);

  // The Variable undergoing calculation.
  const GlobalVariable *Gv;

  // Hold the Variable hash value.
  HashCalculator GvHash;
};

} // end namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_REPO_HASH_CALCULATOR_H
