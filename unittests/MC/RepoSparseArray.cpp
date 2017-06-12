//===- llvm/unittests/MC/RepoSparseArray.cpp ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoFragment/MCRepoSparseArray.h"
#include <array>
#include <set>
#include <vector>
#include "gmock/gmock.h"

using namespace llvm::repo;

TEST(RepoSparseArray, InitializerListIndicesHasIndex) {
  auto Arrp = SparseArray<int>::make_unique({0, 2, 4});
  std::array<bool, 256> Indices{{true, false, true, false, true}};

  for (auto Ctr = 0U; Ctr < Indices.size(); ++Ctr) {
    EXPECT_EQ(Arrp->has_index(Ctr), Indices[Ctr]);
  }
}

TEST(RepoSparseArray, InitializeWithIndexAndValue) {
  auto Arrp = SparseArray<int>::make_unique({0, 2, 4}, {1, 2, 3});
  auto &Arr = *Arrp;

  EXPECT_EQ(Arr.size(), 3U);
  EXPECT_TRUE(Arr.has_index(0));
  EXPECT_FALSE(Arr.has_index(1));
  EXPECT_TRUE(Arr.has_index(2));
  EXPECT_FALSE(Arr.has_index(3));
  EXPECT_FALSE(Arr.has_index(256));
  EXPECT_EQ(Arr[0], 1);
  EXPECT_EQ(Arr[2], 2);
  EXPECT_EQ(Arr[4], 3);
}

TEST(RepoSparseArray, Assign) {
  auto Arrp = SparseArray<int>::make_unique({0, 2, 4});
  auto &Arr = *Arrp;

  Arr[0] = 3;
  Arr[2] = 5;
  Arr[4] = 7;

  EXPECT_EQ(Arr[0], 3);
  EXPECT_EQ(Arr[2], 5);
  EXPECT_EQ(Arr[4], 7);
  Arr[4] = 11;
  EXPECT_EQ(Arr[0], 3);
  EXPECT_EQ(Arr[2], 5);
  EXPECT_EQ(Arr[4], 11);
}

TEST(RepoSparseArray, IndexInitializationList) {
  std::string Empty;

  auto Arr = SparseArray<std::string>::make_unique({0, 2, 4});
  for (std::string const &V : *Arr) {
    EXPECT_EQ(V, Empty);
  }

  EXPECT_EQ(std::get<2>(*Arr), Empty);
}

namespace {
    struct CtorCounter {
        CtorCounter () : V {Ctors++} {}
        unsigned V;
        static unsigned Ctors;
    };
    unsigned CtorCounter::Ctors;
}

TEST(RepoSparseArray, IndexInitializationListCtorCheck) {
  CtorCounter::Ctors = 0;

  auto Arrp = SparseArray<CtorCounter>::make_unique({0, 2, 4});
  auto & Arr = *Arrp;
  EXPECT_EQ (Arr [0].V, 0);
  EXPECT_EQ (Arr [2].V, 1);
  EXPECT_EQ (Arr [4].V, 2);
}

TEST(RepoSparseArray, IteratorInitialization) {
  std::array<std::size_t, 3> I1{{0, 2, 4}};
  std::array<int, 3> V1{{1, 2, 3}};

  auto Arrp = SparseArray<int>::make_unique(std::begin(I1), std::end(I1),
                                        std::begin(V1), std::end(V1));
  auto &Arr = *Arrp;

  EXPECT_EQ(Arr[0], 1);
  EXPECT_FALSE(Arr.has_index(1));
  EXPECT_EQ(Arr[2], 2);
  EXPECT_FALSE(Arr.has_index(3));
  EXPECT_EQ(Arr[4], 3);
}

TEST(RepoSparseArray, IteratorInitializationTooFewValues) {
  std::vector<std::size_t> I1{0, 2, 4};
  std::vector<int> V1{1};

  auto Arrp = SparseArray<int>::make_unique(std::begin(I1), std::end(I1),
                                        std::begin(V1), std::end(V1));
  auto &Arr = *Arrp;

  EXPECT_EQ(Arr[0], 1);
  EXPECT_FALSE(Arr.has_index(1));
  EXPECT_EQ(Arr[2], 0);
  EXPECT_FALSE(Arr.has_index(3));
  EXPECT_EQ(Arr[4], 0);
}

TEST(RepoSparseArray, IteratorInitializationTooManyValues) {
  std::vector<std::size_t> I1{3, 5};
  std::vector<int> V1{3, 5, 7};

  auto Arrp = SparseArray<int>::make_unique(std::begin(I1), std::end(I1),
                                        std::begin(V1), std::end(V1));
  auto &Arr = *Arrp;

  EXPECT_FALSE(Arr.has_index(0));
  EXPECT_FALSE(Arr.has_index(1));
  EXPECT_FALSE(Arr.has_index(2));
  EXPECT_EQ(Arr[3], 3);
  EXPECT_FALSE(Arr.has_index(4));
  EXPECT_EQ(Arr[5], 5);
}

TEST(RepoSparseArray, PairInitialization) {
  std::vector<std::pair<std::size_t, char const *>> const Src{
      {0, "zero"}, {2, "two"}, {4, "four"}};
  auto Arrp = SparseArray<std::string>::make_unique(std::begin(Src), std::end(Src));
  auto &Arr = *Arrp;

  EXPECT_EQ(Arr[0], "zero");
  EXPECT_FALSE(Arr.has_index(1));
  EXPECT_EQ(Arr[2], "two");
  EXPECT_FALSE(Arr.has_index(3));
  EXPECT_EQ(Arr[4], "four");
}

TEST(RepoSparseArray, Iterator) {
  auto Arr = SparseArray<char const *>::make_unique(
      {{0, "zero"}, {2, "two"}, {4, "four"}});

  std::vector<std::string> actual;
  std::copy(std::begin(*Arr), std::end(*Arr), std::back_inserter(actual));

  std::vector<std::string> const expected{"zero", "two", "four"};
  EXPECT_THAT(actual, ::testing::ContainerEq(expected));
}

TEST(RepoSparseArray, ReverseIterator) {
  auto Arr = SparseArray<char const *>::make_unique(
      {{0, "zero"}, {2, "two"}, {4, "four"}});

  std::vector<std::string> Actual;
  std::copy(Arr->crbegin(), Arr->crend(), std::back_inserter(Actual));

  std::vector<std::string> const expected{"four", "two", "zero"};
  EXPECT_THAT(Actual, ::testing::ContainerEq(expected));
}

TEST(RepoSparseArray, Fill) {
  auto Arr =
      SparseArray<std::string>::make_unique({{0, "zero"}, {2, "two"}, {4, "four"}});
  Arr->fill("foo");

  std::vector<std::string> Actual;
  std::copy(std::begin(*Arr), std::end(*Arr), std::back_inserter(Actual));
  std::vector<std::string> const expected{"foo", "foo", "foo"};
  EXPECT_THAT(Actual, ::testing::ContainerEq(expected));
}

TEST(RepoSparseArray, Equal) {
  auto Arr1 = SparseArray<int>::make_unique({{0, 0}, {2, 2}, {4, 4}});
  auto Arr2 = SparseArray<int>::make_unique({{0, 0}, {2, 2}, {4, 4}});
  EXPECT_TRUE(*Arr1 == *Arr2);
}

TEST(RepoSparseArray, Equal2) {
  auto Arr1 = SparseArray<int>::make_unique({{0, 0}, {2, 2}, {4, 5}});
  auto Arr2 = SparseArray<int>::make_unique({{0, 0}, {2, 2}, {4, 4}});
  EXPECT_FALSE(*Arr1 == *Arr2);
}

TEST(RepoSparseArray, Equal3) {
  auto Arr1 = SparseArray<int>::make_unique({{0, 1}, {2, 2}, {5, 4}});
  auto Arr2 = SparseArray<int>::make_unique({{0, 0}, {2, 2}, {4, 4}});
  EXPECT_FALSE(*Arr1 == *Arr2);
}

TEST(RepoSparseArray, HasIndex) {
  std::set<std::size_t> Indices{2, 3, 5, 7};
  auto Arr =
      SparseArray<int>::make_unique(std::begin(Indices), std::end(Indices), {});
  EXPECT_EQ(Arr->has_index(0), Indices.find(0) != Indices.end());
  EXPECT_EQ(Arr->has_index(1), Indices.find(1) != Indices.end());
  EXPECT_EQ(Arr->has_index(2), Indices.find(2) != Indices.end());
  EXPECT_EQ(Arr->has_index(3), Indices.find(3) != Indices.end());
  EXPECT_EQ(Arr->has_index(4), Indices.find(4) != Indices.end());
  EXPECT_EQ(Arr->has_index(5), Indices.find(5) != Indices.end());
  EXPECT_EQ(Arr->has_index(6), Indices.find(6) != Indices.end());
  EXPECT_EQ(Arr->has_index(7), Indices.find(7) != Indices.end());
}

TEST(RepoSparseArray, Indices) {
  std::set<std::size_t> Indices{2, 3, 5, 7};
  auto Arr =
      SparseArray<int>::make_unique(std::begin(Indices), std::end(Indices), {});

  SparseArray<int>::Indices Idc{*Arr};
  std::vector<std::size_t> Actual;
  std::copy(std::begin(Idc), std::end(Idc), std::back_inserter(Actual));
  std::vector<std::size_t> const Expected{2, 3, 5, 7};
  EXPECT_THAT(Actual, ::testing::ContainerEq(Expected));
}

TEST(RepoSparseArray, SizeBytesAgree) {
  std::vector<std::pair<int, int>> Empty;
  EXPECT_EQ(SparseArray<int>::make_unique(std::begin(Empty), std::end(Empty))
                ->size_bytes(),
            SparseArray<int>::size_bytes(0));

  EXPECT_EQ(SparseArray<int>::make_unique({0})->size_bytes(),
            SparseArray<int>::size_bytes(1));
  EXPECT_EQ(SparseArray<int>::make_unique({1, 3})->size_bytes(),
            SparseArray<int>::size_bytes(2));
  EXPECT_EQ(SparseArray<int>::make_unique({1, 3, 5, 7, 11})->size_bytes(),
            SparseArray<int>::size_bytes(5));
}
//eof:llvm/unittests/MC/RepoSparseArray.cpp
