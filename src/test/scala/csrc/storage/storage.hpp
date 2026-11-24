// SPDX-License-Identifier: MulanPSL-2.0
#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <type_traits>

namespace intention {
constexpr std::uint64_t mask(unsigned width) {
  return width == 64 ? ~std::uint64_t{0} : (std::uint64_t{1} << width) - 1;
}

/** Layout and call order are fixed by the validated IR at code generation.
  * Reads observe committed state; the generated cycle function orders updates.
  */
template<std::size_t Entries, unsigned Width> class StorageModel {
  static_assert(Entries > 0 && Width > 0 && Width <= 64, "invalid storage shape");
public:
  using Entry = std::conditional_t<(Width <= 8), std::uint8_t,
    std::conditional_t<(Width <= 16), std::uint16_t,
    std::conditional_t<(Width <= 32), std::uint32_t, std::uint64_t>>>;
  using State = std::array<Entry, Entries>;
private:
  State state_{};
  template<class Function, class... Parameters>
  std::array<std::uint64_t, Entries> evaluate(Function function, Parameters... parameters) const {
    if constexpr (std::is_invocable_v<Function, const State&, Parameters...>) {
      return function(state_, parameters...);
    } else {
      std::array<std::uint64_t, Entries> result{};
      for (std::size_t i = 0; i < Entries; ++i) result[i] = function(state_[i], parameters...);
      return result;
    }
  }
public:
  std::uint64_t read(std::uint64_t index) const {
    return index < Entries ? state_[static_cast<std::size_t>(index)] : 0;
  }
  void reset(const State &initial) {
    for (std::size_t i = 0; i < Entries; ++i)
      state_[i] = static_cast<Entry>(initial[i] & mask(Width));
  }
  void resetMasked(const State &initial, const State &masks) {
    for (std::size_t i = 0; i < Entries; ++i)
      state_[i] = static_cast<Entry>((state_[i] & ~masks[i]) | (initial[i] & masks[i]));
  }
  template<class Function>
  std::array<std::uint64_t, Entries> foreach(Function function) const {
    return evaluate(function);
  }
  template<class Function>
  void update(const StorageModel &current, bool enabled, Function function) {
    if (!enabled) return;
    const auto values = current.evaluate(function);
    for (std::size_t i = 0; i < Entries; ++i) write<0, Width>(true, i, values[i]);
  }
  template<class Predicate, class... Parameters>
  bool camAny(Predicate predicate, Parameters... parameters) const {
    const auto values = evaluate(predicate, parameters...);
    bool result = false;
    for (std::size_t i = 0; i < Entries; ++i)
      result |= values[i] != 0;
    return result;
  }
  template<unsigned Offset, unsigned FieldWidth, std::size_t Base = 0>
  void writeMask(bool enabled, std::uint64_t selected, std::uint64_t value) {
    if (!enabled) return;
    while (selected != 0) {
      const auto bit = static_cast<unsigned>(__builtin_ctzll(selected));
      write<Offset, FieldWidth>(true, Base + bit, value);
      selected &= selected - 1;
    }
  }
  template<class Predicate, class... Parameters>
  std::array<bool, Entries> camHitVec(Predicate predicate, Parameters... parameters) const {
    const auto values = evaluate(predicate, parameters...);
    std::array<bool, Entries> result{};
    for (std::size_t i = 0; i < Entries; ++i) result[i] = values[i] != 0;
    return result;
  }
  template<class Predicate, class... Parameters>
  std::array<std::uint64_t, (Entries + 63) / 64>
  camHitMask(Predicate predicate, Parameters... parameters) const {
    const auto hits = camHitVec(predicate, parameters...);
    std::array<std::uint64_t, (Entries + 63) / 64> result{};
    std::size_t i = 0;
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    // Gather eight canonical boolean bytes into eight adjacent mask bits.
    if (sizeof(bool) == 1) {
      for (; i + 8 <= Entries; i += 8) {
        std::uint64_t bytes;
        std::memcpy(&bytes, hits.data() + i, 8);
        const auto packed = ((bytes & UINT64_C(0x0101010101010101)) *
          UINT64_C(0x0102040810204080)) >> 56;
        result[i / 64] |= packed << (i % 64);
      }
    }
#endif
    for (; i < Entries; ++i) result[i / 64] |= std::uint64_t(hits[i]) << (i % 64);
    return result;
  }
  template<class Predicate, class... Parameters>
  std::array<std::uint64_t, Entries> camHitIdx(Predicate predicate, Parameters... parameters) const {
    const auto values = evaluate(predicate, parameters...);
    std::array<std::uint64_t, Entries> result{};
    result.fill(Entries);
    std::size_t rank = 0;
    for (std::size_t i = 0; i < Entries; ++i)
      if (values[i] != 0) result[rank++] = i;
    return result;
  }
  template<std::size_t MaxHits> struct Matches {
    std::array<std::uint64_t, MaxHits> values{};
    std::array<bool, MaxHits> hits{};
    bool overflow = false;
  };
  template<std::size_t MaxHits, class Predicate, class... Parameters>
  Matches<MaxHits> camRead(Predicate predicate, Parameters... parameters) const {
    static_assert(MaxHits > 0 && MaxHits <= Entries, "invalid CAM hit bound");
    const auto values = evaluate(predicate, parameters...);
    Matches<MaxHits> result;
    std::size_t rank = 0;
    for (std::size_t i = 0; i < Entries; ++i) {
      if (values[i] == 0) continue;
      if (rank == MaxHits) { result.overflow = true; continue; }
      result.values[rank] = read(i); result.hits[rank] = true; ++rank;
    }
    return result;
  }
  template<unsigned Offset, unsigned FieldWidth, class Predicate, class... Parameters>
  void camWrite(const StorageModel &current, bool enabled, std::uint64_t value,
                Predicate predicate, Parameters... parameters) {
    if (!enabled) return;
    const auto matches = current.evaluate(predicate, parameters...);
    for (std::size_t i = 0; i < Entries; ++i)
      write<Offset, FieldWidth>(matches[i] != 0, i, value);
  }
  template<unsigned Offset, unsigned FieldWidth>
  void write(bool enabled, std::uint64_t index, std::uint64_t value) {
    static_assert(FieldWidth > 0 && Offset + FieldWidth <= Width, "invalid storage field");
    if (!enabled || index >= Entries) return;
    auto &word = state_[static_cast<std::size_t>(index)];
    constexpr auto field = mask(FieldWidth);
    if (Offset == 0 && FieldWidth == Width) {
      word = static_cast<Entry>(value & field);
    } else {
      constexpr auto shifted = field << Offset;
      word = static_cast<Entry>((std::uint64_t{word} & ~shifted) | ((value & field) << Offset));
    }
  }
};

} // namespace intention
