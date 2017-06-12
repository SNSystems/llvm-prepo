//===- llvm/unittests/MC/RepoAligned.cpp ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoFragment/MCRepoAligned.h"
#include "gmock/gmock.h"

using namespace llvm::repo;

TEST(RepoAligned, PowerOf2) {
  EXPECT_FALSE(isPowerOfTwo(0U));
  EXPECT_TRUE(isPowerOfTwo(1U));
  EXPECT_TRUE(isPowerOfTwo(2U));
  EXPECT_FALSE(isPowerOfTwo(3U));
  EXPECT_FALSE(isPowerOfTwo(255U));
  EXPECT_TRUE(isPowerOfTwo(256U));
}

TEST(RepoAligned, AlignedInt4) {
  EXPECT_EQ(0, aligned(0, 4));
  EXPECT_EQ(4, aligned(1, 4));
  EXPECT_EQ(4, aligned(2, 4));
  EXPECT_EQ(4, aligned(3, 4));
  EXPECT_EQ(4, aligned(4, 4));
  EXPECT_EQ(8, aligned(5, 4));
}
//eof:llvm/unittests/MC/RepoAligned.cpp
