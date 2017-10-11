//===- RepoHashCalculator.cpp - Unit tests for RepoHashCalculator ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "llvm/Transforms/Utils/RepoHashCalculator.h"
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

/// A class for testing the RepoHashCalculator API.
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

  Digest::DigestType testHashSignature() {
    FnHash.beginCalculate();
    hashSignature(Fn);
    return getHash();
  }

  Digest::DigestType testHashBasicBlock(const BasicBlock *BB) {
    FnHash.beginCalculate();
    hashBasicBlock(BB);
    return getHash();
  }

  Digest::DigestType testHashConstant(const Constant *V) {
    FnHash.beginCalculate();
    FnHash.hashConstant(V);
    return getHash();
  }

  Digest::DigestType testHashGlobalValue(const GlobalValue *V) {
    FnHash.beginCalculate();
    FnHash.hashGlobalValue(V);
    return getHash();
  }

  Digest::DigestType testHashValue(const Value *V) {
    FnHash.beginCalculate();
    FnHash.hashValue(V);
    return getHash();
  }

  Digest::DigestType testHashInstruction(const Instruction *V) {
    FnHash.beginCalculate();
    hashInstruction(V);
    return getHash();
  }

  Digest::DigestType testHashType(Type *Ty) {
    FnHash.beginCalculate();
    FnHash.hashType(Ty);
    return getHash();
  }

  Digest::DigestType testHashNumber(uint64_t V) {
    FnHash.beginCalculate();
    FnHash.hashNumber(V);
    return getHash();
  }

  Digest::DigestType testHashAPInt(const APInt &V) {
    FnHash.beginCalculate();
    FnHash.hashAPInt(V);
    return getHash();
  }

  Digest::DigestType testHashAPFloat(const APFloat &V) {
    FnHash.beginCalculate();
    FnHash.hashAPFloat(V);
    return getHash();
  }

  Digest::DigestType testHashMem(StringRef V) {
    FnHash.beginCalculate();
    FnHash.hashMem(V);
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
  EXPECT_EQ(F1H.testHashSignature(), F2H.testHashSignature());
  EXPECT_NE(F1H.testHashBasicBlock(F1.BB), F2H.testHashBasicBlock(F2.BB));
  EXPECT_EQ(F1H.testHashBasicBlock(F1.BB), F3H.testHashBasicBlock(F3.BB));
  EXPECT_NE(F1H.testHashConstant(F1.C), F2H.testHashConstant(F2.C));

  EXPECT_NE(F1H.testHashGlobalValue(F1.F), F2H.testHashGlobalValue(F2.F));
  EXPECT_NE(F1H.testHashInstruction(F1.I), F2H.testHashInstruction(F2.I));
  EXPECT_EQ(F1H.testHashType(F1.T), F2H.testHashType(F2.T));

  EXPECT_EQ(F1H.testHashNumber(5), F2H.testHashNumber(5));
  EXPECT_NE(F1H.testHashNumber(5), F2H.testHashNumber(6));
  EXPECT_EQ(F1H.testHashMem("Hello World"), F2H.testHashMem("Hello World"));
  EXPECT_NE(F1H.testHashMem("Hello World"), F2H.testHashMem("Hello-World"));
  EXPECT_EQ(F1H.testHashAPInt(APInt(32, 2)), F2H.testHashAPInt(APInt(32, 2)));
  EXPECT_NE(F1H.testHashAPInt(APInt(32, 2)), F2H.testHashAPInt(APInt(32, 5)));
  EXPECT_EQ(F1H.testHashAPFloat(APFloat(2.0)),
            F2H.testHashAPFloat(APFloat(2.0)));
  EXPECT_NE(F1H.testHashAPFloat(APFloat(2.0)),
            F2H.testHashAPFloat(APFloat(5.0)));
}
