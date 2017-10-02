//===- HashCalculator.cpp - Unit tests for HashCalculator ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "llvm/Transforms/Utils/HashCalculator.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoTicket.h"
#include "gtest/gtest.h"

using namespace llvm;

/// Generates a simple test function.
struct TestFunction {
  Function *F;
  BasicBlock *BB;
  Constant *C;
  Instruction *I;
  Type *T;

  TestFunction(LLVMContext &Ctx, Module &M, int addVal) {
    IRBuilder<> B(Ctx);
    T = B.getInt8Ty();
    F = Function::Create(FunctionType::get(T, {B.getInt8PtrTy()}, false),
                         GlobalValue::ExternalLinkage, "F", &M);
    BB = BasicBlock::Create(Ctx, "", F);
    B.SetInsertPoint(BB);
    Argument *PointerArg = &*F->arg_begin();
    LoadInst *LoadInst = B.CreateLoad(PointerArg);
    C = B.getInt8(addVal);
    I = cast<Instruction>(B.CreateAdd(LoadInst, C));
    B.CreateRet(I);
  }
};

/// A class for testing the HashCalculator API.
///
/// The main purpose is to test if the required protected functions are
/// accessible from a derived class of FunctionComparator.
class TestHash : public FunctionHashCalculator {
public:
  TestHash(const Function *F1) : FunctionHashCalculator(F1) {}

  bool testFunctionAccess(const Function *F1) {
    // Test if Fn is accessible.
    return F1 == Fn;
  }

  Digest::DigestType getHash() { return getHashResult(); }

  Digest::DigestType testCalculate(Module &M) {
    calculateHash(M);
    return getHash();
  }

  Digest::DigestType testSignatureHash() {
    FnHash.beginCalculate();
    signatureHash(Fn);
    return getHash();
  }

  Digest::DigestType testBasicBlockHash(const BasicBlock *BB) {
    FnHash.beginCalculate();
    basicBlockHash(BB);
    return getHash();
  }

  Digest::DigestType testConstantHash(const Constant *V) {
    FnHash.beginCalculate();
    FnHash.constantHash(V);
    return getHash();
  }

  Digest::DigestType testGlobalValueHash(const GlobalValue *V) {
    FnHash.beginCalculate();
    FnHash.globalValueHash(V);
    return getHash();
  }

  Digest::DigestType testValueHash(const Value *V) {
    FnHash.beginCalculate();
    FnHash.valueHash(V);
    return getHash();
  }

  Digest::DigestType testInstructionHash(const Instruction *V) {
    FnHash.beginCalculate();
    instructionHash(V);
    return getHash();
  }

  Digest::DigestType testTypeHash(Type *Ty) {
    FnHash.beginCalculate();
    FnHash.typeHash(Ty);
    return getHash();
  }

  Digest::DigestType testNumberHash(uint64_t V) {
    FnHash.beginCalculate();
    FnHash.numberHash(V);
    return getHash();
  }

  Digest::DigestType testAPIntHash(const APInt &V) {
    FnHash.beginCalculate();
    FnHash.APIntHash(V);
    return getHash();
  }

  Digest::DigestType testAPFloatHash(const APFloat &V) {
    FnHash.beginCalculate();
    FnHash.APFloatHash(V);
    return getHash();
  }

  Digest::DigestType testMemHash(StringRef V) {
    FnHash.beginCalculate();
    FnHash.memHash(V);
    return getHash();
  }
};

/// A sanity check for the FunctionComparator API.
TEST(HashCalculatorTest, TestAPI) {
  LLVMContext C;
  Module M("test", C);
  TestFunction F1(C, M, 27);
  TestFunction F2(C, M, 28);
  TestFunction F3(C, M, 27);

  TestHash F1H(F1.F);
  TestHash F2H(F2.F);
  TestHash F3H(F3.F);

  EXPECT_TRUE(F1H.testFunctionAccess(F1.F));
  EXPECT_NE(F1H.testCalculate(M), F2H.testCalculate(M));
  EXPECT_EQ(F1H.testCalculate(M), F3H.testCalculate(M));
  EXPECT_EQ(F1H.testSignatureHash(), F2H.testSignatureHash());
  EXPECT_NE(F1H.testBasicBlockHash(F1.BB), F2H.testBasicBlockHash(F2.BB));
  EXPECT_EQ(F1H.testBasicBlockHash(F1.BB), F3H.testBasicBlockHash(F3.BB));
  EXPECT_NE(F1H.testConstantHash(F1.C), F2H.testConstantHash(F2.C));

  EXPECT_NE(F1H.testGlobalValueHash(F1.F), F2H.testGlobalValueHash(F2.F));
  EXPECT_NE(F1H.testInstructionHash(F1.I), F2H.testInstructionHash(F2.I));
  EXPECT_EQ(F1H.testTypeHash(F1.T), F2H.testTypeHash(F2.T));

  EXPECT_EQ(F1H.testNumberHash(5), F2H.testNumberHash(5));
  EXPECT_NE(F1H.testNumberHash(5), F2H.testNumberHash(6));
  EXPECT_EQ(F1H.testMemHash("Hello World"), F2H.testMemHash("Hello World"));
  EXPECT_NE(F1H.testMemHash("Hello World"), F2H.testMemHash("Hello-World"));
  EXPECT_EQ(F1H.testAPIntHash(APInt(32, 2)), F2H.testAPIntHash(APInt(32, 2)));
  EXPECT_NE(F1H.testAPIntHash(APInt(32, 2)), F2H.testAPIntHash(APInt(32, 5)));
  EXPECT_EQ(F1H.testAPFloatHash(APFloat(2.0)),
            F2H.testAPFloatHash(APFloat(2.0)));
  EXPECT_NE(F1H.testAPFloatHash(APFloat(2.0)),
            F2H.testAPFloatHash(APFloat(5.0)));
}
