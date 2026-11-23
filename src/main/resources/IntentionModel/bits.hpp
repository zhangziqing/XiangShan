// SPDX-License-Identifier: MulanPSL-2.0
#pragma once

#include <cstdint>
#include <type_traits>

namespace intention {
namespace bits_detail {
constexpr unsigned maximum(unsigned a, unsigned b) { return a > b ? a : b; }
constexpr unsigned dynamic_left_width(unsigned width, unsigned shift_width) {
  return shift_width >= 8 ? 129 : width + (unsigned{1} << shift_width) - 1;
}
} // namespace bits_detail

/** Two-state FIRRTL bit vectors. Width and signedness are compile-time metadata.
  * Construction/assignment keeps the low Width bits; widening extends the source
  * sign. Casts reinterpret bits. Arithmetic uses unsigned host operations only.
  * Operators retain FIRRTL intermediate widths, including the carry in add/sub.
  * Public packed callbacks use at most 64 bits; intermediates use at most 128.
  */
template<unsigned Width, bool Signed = false> class Bits {
  static_assert(Width > 0 && Width <= 128, "Bits width must be in 1..128");
  using Raw = __uint128_t;
  using Storage = std::conditional_t<(Width <= 64), std::uint64_t, Raw>;
  Storage value_{};
  static constexpr Raw bit_mask() { return ~Raw{0} >> (128 - Width); }
  static constexpr Storage normalize(Raw value) {
    return static_cast<Storage>(value & bit_mask());
  }
  constexpr Raw left(Raw count) const {
    return count >= 128 ? Raw{0} : extended() << unsigned(count);
  }
  constexpr Raw right(Raw count) const {
    if (count >= 128) return negative() ? ~Raw{0} : Raw{0};
    const unsigned shift = unsigned(count);
    return (extended() >> shift) |
      (negative() && shift != 0 ? ~Raw{0} << (128 - shift) : Raw{0});
  }
  constexpr bool negative() const { return Signed && ((raw() >> (Width - 1)) & 1); }
  constexpr Raw ordered() const {
    return Signed ? extended() ^ (Raw{1} << 127) : raw();
  }
public:
  static constexpr unsigned width = Width;
  static constexpr bool is_signed = Signed;
  constexpr Bits() = default;
  constexpr explicit Bits(Raw value) : value_(normalize(value)) {}
  template<unsigned Other, bool Sign>
  constexpr Bits(const Bits<Other, Sign> &other) : value_(normalize(other.extended())) {}
  static constexpr Bits from_words(std::uint64_t high, std::uint64_t low) {
    return Bits{(Raw{high} << 64) | Raw{low}};
  }
  constexpr Raw raw() const { return Raw{value_}; }
  constexpr Raw extended() const {
    const Raw sign = Raw{1} << (Width - 1);
    return Signed ? (raw() ^ sign) - sign : raw();
  }
  constexpr explicit operator bool() const { return raw() != 0; }
  constexpr std::uint64_t to_uint64() const {
    static_assert(Width <= 64, "slice or truncate a wide value before exporting it");
    return static_cast<std::uint64_t>(value_);
  }
  constexpr Bits &assign(Raw value) {
    value_ = normalize(value); return *this;
  }
  template<unsigned Other, bool Sign>
  constexpr Bits &assign(const Bits<Other, Sign> &other) { return assign(other.extended()); }
  constexpr Bits &operator=(Raw value) { return assign(value); }
  template<unsigned Other, bool Sign>
  constexpr Bits &operator=(const Bits<Other, Sign> &other) { return assign(other); }

  template<unsigned Other>
  constexpr Bits<bits_detail::maximum(Width, Other) + 1, Signed>
  operator+(const Bits<Other, Signed> &other) const {
    return Bits<bits_detail::maximum(Width, Other) + 1, Signed>{extended() + other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<bits_detail::maximum(Width, Other) + 1, Signed>
  operator-(const Bits<Other, Signed> &other) const {
    return Bits<bits_detail::maximum(Width, Other) + 1, Signed>{extended() - other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<Width + Other, Signed> operator*(const Bits<Other, Signed> &other) const {
    return Bits<Width + Other, Signed>{extended() * other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<bits_detail::maximum(Width, Other)>
  operator&(const Bits<Other, Signed> &other) const {
    return Bits<bits_detail::maximum(Width, Other)>{extended() & other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<bits_detail::maximum(Width, Other)>
  operator|(const Bits<Other, Signed> &other) const {
    return Bits<bits_detail::maximum(Width, Other)>{extended() | other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<bits_detail::maximum(Width, Other)>
  operator^(const Bits<Other, Signed> &other) const {
    return Bits<bits_detail::maximum(Width, Other)>{extended() ^ other.extended()};
  }
  constexpr Bits<Width> operator~() const { return Bits<Width>{~raw()}; }
  constexpr Bits<Width + 1, true> operator-() const {
    return Bits<Width + 1, true>{Raw{0} - extended()};
  }
  template<unsigned Other>
  constexpr Bits<1> operator==(const Bits<Other, Signed> &other) const {
    return Bits<1>{extended() == other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<1> operator!=(const Bits<Other, Signed> &other) const {
    return Bits<1>{extended() != other.extended()};
  }
  template<unsigned Other>
  constexpr Bits<1> operator<(const Bits<Other, Signed> &other) const {
    const Raw rhs = Signed ? other.extended() ^ (Raw{1} << 127) : other.raw();
    return Bits<1>{ordered() < rhs};
  }
  template<unsigned Other>
  constexpr Bits<1> operator<=(const Bits<Other, Signed> &other) const {
    return Bits<1>{!static_cast<bool>(other < *this)};
  }
  template<unsigned Other>
  constexpr Bits<1> operator>(const Bits<Other, Signed> &other) const { return other < *this; }
  template<unsigned Other>
  constexpr Bits<1> operator>=(const Bits<Other, Signed> &other) const {
    return Bits<1>{!static_cast<bool>(*this < other)};
  }

  template<unsigned High, unsigned Low>
  constexpr Bits<High - Low + 1> slice() const {
    static_assert(Low <= High && High < Width, "invalid Bits slice");
    return Bits<High - Low + 1>{raw() >> Low};
  }
  template<unsigned Count> constexpr Bits<Count> head() const {
    static_assert(Count > 0 && Count <= Width, "invalid Bits head");
    return slice<Width - 1, Width - Count>();
  }
  template<unsigned Count> constexpr Bits<Width - Count> tail() const {
    static_assert(Count < Width, "invalid Bits tail");
    return Bits<Width - Count>{raw()};
  }
  constexpr Bits<Width> as_uint() const { return Bits<Width>{raw()}; }
  constexpr Bits<Width, true> as_sint() const { return Bits<Width, true>{raw()}; }
  constexpr Bits<Width + (Signed ? 0 : 1), true> cvt() const { return *this; }
  template<unsigned Target>
  constexpr Bits<bits_detail::maximum(Width, Target), Signed> pad() const { return *this; }
  template<unsigned Other, bool Sign>
  constexpr Bits<Width + Other> concat(const Bits<Other, Sign> &other) const {
    return Bits<Width + Other>{(raw() << Other) | other.raw()};
  }
  template<unsigned Count> constexpr Bits<Width + Count, Signed> shl() const {
    return Bits<Width + Count, Signed>{left(Count)};
  }
  template<unsigned Count>
  constexpr Bits<(Count < Width ? Width - Count : 1), Signed> shr() const {
    return Bits<(Count < Width ? Width - Count : 1), Signed>{right(Count)};
  }
  template<unsigned Other>
  constexpr Bits<bits_detail::dynamic_left_width(Width, Other), Signed>
  operator<<(const Bits<Other> &count) const {
    return Bits<bits_detail::dynamic_left_width(Width, Other), Signed>{left(count.raw())};
  }
  template<unsigned Other> constexpr Bits operator>>(const Bits<Other> &count) const {
    return Bits{right(count.raw())};
  }
  template<unsigned Other> constexpr Bits &operator<<=(const Bits<Other> &count) {
    return assign(left(count.raw()));
  }
  template<unsigned Other> constexpr Bits &operator>>=(const Bits<Other> &count) {
    return assign(right(count.raw()));
  }
  constexpr Bits<1> all() const { return Bits<1>{raw() == bit_mask()}; }
  constexpr Bits<1> any() const { return Bits<1>{raw() != 0}; }
  constexpr Bits<1> parity() const {
    Raw value = raw();
    value ^= value >> 64; value ^= value >> 32; value ^= value >> 16;
    value ^= value >> 8; value ^= value >> 4; value ^= value >> 2; value ^= value >> 1;
    return Bits<1>{value & 1};
  }
};

template<unsigned Left, unsigned Right, bool Signed>
constexpr Bits<bits_detail::maximum(Left, Right), Signed>
mux(Bits<1> condition, const Bits<Left, Signed> &yes, const Bits<Right, Signed> &no) {
  using Result = Bits<bits_detail::maximum(Left, Right), Signed>;
  return condition ? Result{yes} : Result{no};
}
} // namespace intention
