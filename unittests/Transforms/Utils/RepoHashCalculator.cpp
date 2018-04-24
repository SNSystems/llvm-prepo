//===- RepoHashCalculator.cpp - Unit tests for RepoHashCalculator ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoHashCalculator.h"
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
/// accessible from a derived class of FunctionHashCalculator.
class TestHash : public FunctionHashCalculator {
public:
  TestHash(const Function *F1) : FunctionHashCalculator(F1) {}

  bool testFunctionAccess(const Function *F1) { return F1 == function(); }

  ticketmd::DigestType getHash() { return getHashResult(); }

  ticketmd::DigestType testCalculate() {
    calculateHash();
    return getHash();
  }

  ticketmd::DigestType testHashSignature() {
    hasher().beginCalculate(*function()->getParent());
    hashSignature(function());
    return getHash();
  }

  ticketmd::DigestType testHashBasicBlock(const BasicBlock *BB) {
    hasher().beginCalculate(*function()->getParent());
    hashBasicBlock(BB);
    return getHash();
  }

  ticketmd::DigestType testHashConstant(const Constant *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashConstant(V);
    return getHash();
  }

  ticketmd::DigestType testHashGlobalValue(const GlobalValue *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashGlobalValue(V);
    return getHash();
  }

  ticketmd::DigestType testHashValue(const Value *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashValue(V);
    return getHash();
  }

  ticketmd::DigestType testHashInstruction(const Instruction *V) {
    hasher().beginCalculate(*function()->getParent());
    hashInstruction(V);
    return getHash();
  }

  ticketmd::DigestType testHashType(Type *Ty) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashType(Ty);
    return getHash();
  }

  ticketmd::DigestType testHashNumber(uint64_t V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashNumber(V);
    return getHash();
  }

  ticketmd::DigestType testHashAPInt(const APInt &V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashAPInt(V);
    return getHash();
  }

  ticketmd::DigestType testHashAPFloat(const APFloat &V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashAPFloat(V);
    return getHash();
  }

  ticketmd::DigestType testHashMem(StringRef V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashMem(V);
    return getHash();
  }
};

/// A test for the FunctionHashCalculator.
TEST(HashCalculatorTest, TestFunction) {
  LLVMContext C;
  Module M("test", C);
  MD5::MD5Result Hash = {};
  M.setModuleHash(Hash);
  TestFunction F1(C, M, 27);
  TestFunction F2(C, M, 28);
  TestFunction F3(C, M, 27);

  TestHash F1H(F1.F);
  TestHash F2H(F2.F);
  TestHash F3H(F3.F);

  EXPECT_TRUE(F1H.testFunctionAccess(F1.F));
  EXPECT_NE(F1H.testCalculate(), F2H.testCalculate());
  EXPECT_EQ(F1H.testCalculate(), F3H.testCalculate());
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

class VariableHash : public testing::Test {
protected:
  VariableHash() {
    M0.reset(new Module("Module", Ctx));
    M1.reset(new Module("Module1", Ctx));
    MD5::MD5Result Hash = {};
    M0->setModuleHash(Hash);
    M1->setModuleHash(Hash);
    GV0 = new GlobalVariable(*M0, Type::getInt8Ty(Ctx), true,
                             GlobalValue::ExternalLinkage, nullptr, "GV0");
    GV1 = new GlobalVariable(*M1, Type::getInt8Ty(Ctx), true,
                             GlobalValue::ExternalLinkage, nullptr, "GV1");
  }

  ~VariableHash() {}

  bool isEqualHash() {
    VariableHashCalculator GVH0{GV0}, GVH1{GV1};
    GVH0.calculateHash();
    GVH1.calculateHash();
    return GVH0.getHashResult() == GVH1.getHashResult();
  }

  LLVMContext Ctx;
  std::unique_ptr<Module> M0;
  std::unique_ptr<Module> M1;
  GlobalVariable *GV0;
  GlobalVariable *GV1;
};

// The module hash will affect the hash value.
TEST_F(VariableHash, SameModuleHash) {
  MD5 Hash;
  Hash.update("foo");
  MD5::MD5Result Result;
  Hash.final(Result);
  M0->setModuleHash(Result);
  M1->setModuleHash(Result);
  EXPECT_TRUE(isEqualHash());
}

TEST_F(VariableHash, DiffModuleHash) {
  MD5 Hash;
  Hash.update("foo");
  MD5::MD5Result Result;
  Hash.final(Result);
  M0->setModuleHash(Result);
  Hash.update("bar");
  Hash.final(Result);
  M1->setModuleHash(Result);
  EXPECT_FALSE(isEqualHash());
}

// The source filename will not affect the hash value.
TEST_F(VariableHash, CheckSourceFile) {
  M0->setSourceFileName("file.cpp");
  M1->setSourceFileName("file1.cpp");
  EXPECT_TRUE(isEqualHash());
}

// The name and linkage type will affect the hash value.
TEST_F(VariableHash, CheckLinkage) {
  GV0->setLinkage(GlobalValue::ExternalLinkage);
  GV1->setLinkage(GlobalValue::InternalLinkage);
  EXPECT_TRUE(isEqualHash());
}

// The alignment will affect the hash value.
TEST_F(VariableHash, CheckAlignment) {
  GV0->setAlignment(1);
  GV1->setAlignment(4);
  EXPECT_FALSE(isEqualHash());
}

// The constant type will affect the hash value.
TEST_F(VariableHash, CheckConstant) {
  GV0->setConstant(false);
  GV1->setConstant(true);
  EXPECT_FALSE(isEqualHash());
}

// The initializr will affect the hash value.
TEST_F(VariableHash, CheckInitializer) {
  Type *Int8 = Type::getInt8Ty(Ctx);

  Constant *Zero = ConstantInt::get(Int8, 0);
  GV0->setInitializer(Zero);
  EXPECT_TRUE(isEqualHash());

  GV1->setInitializer(Zero);
  EXPECT_TRUE(isEqualHash());

  Constant *One = ConstantInt::get(Int8, 1);
  GV1->setInitializer(One);
  EXPECT_FALSE(isEqualHash());
}

// The value type will affect the hash value.
TEST_F(VariableHash, CheckType) {
  GV1 = new GlobalVariable(*M1, Type::getInt32Ty(Ctx), true,
                           GlobalValue::ExternalLinkage, nullptr);
  EXPECT_FALSE(isEqualHash());
}
