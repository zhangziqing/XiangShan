// Native Storage template for software execution.
//
// One function per Storage semantic; the generated adapter owns only a few
// calls. Reads and CAM queries observe committed state, writes are staged and
// committed at the cycle edge. This mirrors utils.QueueModel.Storage:
//   read, write, writeField, camHitVec, camHitIdx, camRead, camWrite,
//   camWriteField, update, foreach, plus begin/commit for cycle staging.
#pragma once

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace storage_boundary {

constexpr uint64_t mask(unsigned width) {
  return width == 64 ? UINT64_MAX : (UINT64_C(1) << width) - 1;
}

constexpr unsigned bit_width(uint64_t value) {
  unsigned width = 0;
  while (value) {
    ++width;
    value >>= 1;
  }
  return width;
}

// Lowered FieldOf path for a scalar word: least-significant bit and width.
struct Field {
  unsigned offset;
  unsigned width;
};

template <size_t Entries, unsigned Width, size_t PendingWrites>
class Storage {
  static_assert(Entries > 0, "Storage capacity must be positive");
  static_assert(Width > 0 && Width <= 64, "Storage word must be 1..64 bits");
  static_assert(PendingWrites > 0, "Storage needs a write staging bound");

public:
  // Matches utils.QueueModel.Storage.camHitIdx width:
  // max(1, ceil(log2(entries + 1))).
  static constexpr unsigned IndexWidth = bit_width(Entries);

  struct Hit {
    uint64_t value;
    bool hit;
  };

  explicit Storage(const std::array<uint64_t, Entries>& initial) : initial_(initial) {
    for (auto& value : initial_) value &= mask(Width);
    storage_ = initial_;
  }

  // Reset discards pending writes and wins at commit.
  void begin(bool reset) {
    pending_count_ = 0;
    reset_ = reset;
  }

  // Out-of-range dynamic reads return zero; static indexing is the caller's
  // responsibility, as in the hardware contract.
  uint64_t read(uint64_t index) const { return index < Entries ? storage_[index] : 0; }

  void write(uint64_t index, uint64_t value) {
    if (index < Entries) stage(index, mask(Width), value);
  }

  void writeField(uint64_t index, Field field, uint64_t value) {
    assert(valid(field));
    if (index < Entries && valid(field)) {
      const uint64_t field_mask = mask(field.width) << field.offset;
      stage(index, field_mask, (value & mask(field.width)) << field.offset);
    }
  }

  template <typename Predicate>
  std::array<bool, Entries> camHitVec(Predicate predicate) const {
    std::array<bool, Entries> result{};
    for (size_t index = 0; index < Entries; ++index) result[index] = predicate(storage_[index]);
    return result;
  }

  // Ascending matches padded with the invalid index Entries.
  template <typename Predicate>
  std::array<uint64_t, Entries> camHitIdx(Predicate predicate) const {
    std::array<uint64_t, Entries> result{};
    size_t count = 0;
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(storage_[index])) result[count++] = index;
    }
    while (count < Entries) result[count++] = Entries;
    return result;
  }

  // Physical-order matches padded with {0, false}; an over-hit is asserted,
  // matching the hardware camRead bound.
  template <size_t MaxHits, typename Predicate>
  std::array<Hit, MaxHits> camRead(Predicate predicate) const {
    static_assert(MaxHits > 0 && MaxHits <= Entries, "camRead maxHits is out of range");
    std::array<Hit, MaxHits> result{};
    size_t count = 0;
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(storage_[index])) {
        assert(count < MaxHits);
        if (count < MaxHits) result[count++] = {storage_[index], true};
      }
    }
    return result;
  }

  template <typename Predicate>
  void camWrite(Predicate predicate, uint64_t value) {
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(storage_[index])) stage(index, mask(Width), value);
    }
  }

  template <typename Predicate>
  void camWriteField(Predicate predicate, Field field, uint64_t value) {
    assert(valid(field));
    if (!valid(field)) return;
    const uint64_t field_mask = mask(field.width) << field.offset;
    const uint64_t field_value = (value & mask(field.width)) << field.offset;
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(storage_[index])) stage(index, field_mask, field_value);
    }
  }

  // Whole-entry replacement from committed state, like Storage.update.
  template <typename Transform>
  void update(Transform transform) {
    for (size_t index = 0; index < Entries; ++index) {
      stage(index, mask(Width), transform(storage_[index]));
    }
  }

  // Read-only mapping in physical order. The result type must be copyable and
  // default-constructible.
  template <typename Transform>
  auto foreach(Transform transform) const {
    using Result = decltype(transform(uint64_t{}));
    std::array<Result, Entries> result{};
    for (size_t index = 0; index < Entries; ++index) result[index] = transform(storage_[index]);
    return result;
  }

  // Later enabled writes win per field; reset restores the initial contents.
  void commit() {
    if (reset_) {
      storage_ = initial_;
    } else {
      for (size_t index = 0; index < pending_count_; ++index) {
        const Write& write = pending_[index];
        storage_[write.index] = (storage_[write.index] & ~write.mask) | write.value;
      }
    }
    pending_count_ = 0;
    reset_ = false;
  }

private:
  struct Write {
    uint64_t index;
    uint64_t mask;
    uint64_t value;
  };

  static bool valid(Field field) {
    return field.width > 0 && field.width <= Width && field.offset + field.width <= Width;
  }

  void stage(uint64_t index, uint64_t field_mask, uint64_t value) {
    assert(pending_count_ < PendingWrites);
    if (pending_count_ < PendingWrites) {
      pending_[pending_count_++] = {index, field_mask, value & field_mask};
    }
  }

  std::array<uint64_t, Entries> storage_{};
  std::array<uint64_t, Entries> initial_{};
  std::array<Write, PendingWrites> pending_{};
  size_t pending_count_ = 0;
  bool reset_ = false;
};

}  // namespace storage_boundary
