//===- llvm/unittests/MC/RepoFragment.cpp ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoFragment/MCRepoFragment.h"
#include "gmock/gmock.h"
#include <vector>

#if 0 // FIXME: Will re-enable the test!
using namespace llvm::repo;

TEST(Fragment, Empty) {
  std::vector<SectionContent> C;
  auto Fragment = ::Fragment::make_unique(std::begin(C), std::end(C));
  EXPECT_EQ(0U, Fragment->numSections ());
}

TEST(Fragment, MakeReadOnlySection) {
  SectionContent RoData{SectionType::ReadOnly};
  RoData.Data.assign({'r', 'o', 'd', 'a', 't', 'a'});

  std::vector<SectionContent> c{RoData};
  auto Fragment = ::Fragment::make_unique(std::begin(c), std::end(c));

  std::vector<std::size_t> const Expected{
      static_cast<std::size_t>(SectionType::ReadOnly)};
  auto Indices = Fragment->sections ().getIndices();
  std::vector<std::size_t> Actual(std::begin(Indices), std::end(Indices));
  EXPECT_THAT(Actual, ::testing::ContainerEq(Expected));

  Section const &S = (*Fragment)[SectionType::ReadOnly];
  auto DataBegin = std::begin(S.data());
  auto DataEnd = std::end(S.data());
  auto RoDataBegin = std::begin(RoData.Data);
  auto RoDataEnd = std::end(RoData.Data);
  ASSERT_EQ (std::distance (DataBegin, DataEnd), std::distance (RoDataBegin, RoDataEnd));
  EXPECT_TRUE(std::equal(DataBegin, DataEnd, RoDataBegin));
  EXPECT_EQ(0U, S.ifixups().size());
  EXPECT_EQ(0U, S.xfixups().size());
}
#endif
//eof:llvm/unittests/MC/RepoFragment.cpp
