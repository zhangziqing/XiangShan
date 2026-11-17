// SPDX-License-Identifier: MulanPSL-2.0
#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
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
public:
  std::uint64_t read(std::uint64_t index) const {
    return index < Entries ? state_[static_cast<std::size_t>(index)] : 0;
  }
  void reset(const State &initial) {
    for (std::size_t i = 0; i < Entries; ++i)
      state_[i] = static_cast<Entry>(initial[i] & mask(Width));
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
