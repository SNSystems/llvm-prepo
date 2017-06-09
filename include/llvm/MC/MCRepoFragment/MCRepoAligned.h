#ifndef LLVM_MCREPOFRAGMENT_ALIGNED_H
#define LLVM_MCREPOFRAGMENT_ALIGNED_H

#include <cassert>
#include <cstdint>
#include <type_traits>

namespace llvm {
namespace repo {

/// \returns True if the input value is a power of 2.
template <typename Ty,
          typename = typename std::enable_if<std::is_unsigned<Ty>::value>>
inline bool isPowerOfTwo(Ty n) {
  //  if a number n is a power of 2 then bitwise & of n and n-1 will be zero.
  return n && !(n & (n - 1U));
}

/// \param V  The value to be aligned.
/// \param Align  The alignment required for 'v'.
/// \returns  The value closest to but greater than or equal to 'V' for which
/// V%Align is zero.
template <typename IntType>
inline IntType aligned(IntType V, std::size_t Align) {
  assert(isPowerOfTwo(Align));
  return (V + Align - 1U) & ~(Align - 1U);
}

/// \param V  The value to be aligned.
/// \returns  The value closest to but greater than or equal to 'V' for which
/// V%alignof(decltype(V)) is zero.
template <typename AlignType, typename IntType,
          typename = std::enable_if<std::is_integral<IntType>::value>>
inline IntType aligned(IntType V) {
  return aligned(V, alignof(AlignType));
}

template <typename PointeeType> inline PointeeType *aligned(void *v) {
  auto p = reinterpret_cast<std::uintptr_t>(v);
  p = aligned(p, alignof(PointeeType));
  return reinterpret_cast<PointeeType *>(p);
}
template <typename PointeeType>
inline PointeeType const *aligned(void const *v) {
  return aligned<PointeeType>(const_cast<void *>(v));
}

template <typename DestPointeeType, typename SrcPointeeType = DestPointeeType>
inline DestPointeeType *alignedPtr(SrcPointeeType *v) {
  return aligned<DestPointeeType>(reinterpret_cast<void *>(v));
}
template <typename DestPointeeType, typename SrcPointeeType = DestPointeeType>
inline DestPointeeType const *alignedPtr(SrcPointeeType const *p) {
  auto v = reinterpret_cast<void const *>(p);
  return aligned<DestPointeeType>(const_cast<void *>(v));
}

} // end namespace repo
} // end namespace llvm

#endif // LLVM_MCREPOFRAGMENT_ALIGNED_H
// eof:Aligned.h
