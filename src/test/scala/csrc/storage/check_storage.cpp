// Differential check for the Storage software template and its adapter.
//
// The template is compared with an independent next-state reference for random
// writes, field writes, CAM writes, updates and resets. The adapter is then
// driven through a small sequence to check the generated call shape.
#include "storage.hpp"

#include <array>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <random>

extern "C" {
void storage_prepare(
    unsigned long long reset,
    unsigned long long write_enable,
    unsigned long long write_index,
    unsigned long long write_value,
    unsigned long long field_enable,
    unsigned long long field_index,
    unsigned long long field_offset,
    unsigned long long field_width,
    unsigned long long field_value,
    unsigned long long cam_enable,
    unsigned long long cam_key,
    unsigned long long cam_value,
    unsigned long long update_enable,
    unsigned long long update_amount);
void storage_read_0(unsigned long long epoch, unsigned long long index, unsigned long long* out);
void storage_read_1(unsigned long long epoch, unsigned long long index, unsigned long long* out);
void storage_query_0(
    unsigned long long epoch, unsigned long long key,
    unsigned long long* out_value, unsigned long long* out_hit);
void storage_query_1(unsigned long long epoch, unsigned long long key, unsigned long long* out_mask);
void storage_query_2(unsigned long long epoch, unsigned long long key, unsigned long long* out_indices);
void storage_snapshot(unsigned long long epoch, unsigned long long* out);
}

namespace {

using storage_boundary::Field;
using storage_boundary::mask;

template <size_t Entries, unsigned Width>
class Reference {
public:
  struct Hit {
    uint64_t value;
    bool hit;
  };

  explicit Reference(const std::array<uint64_t, Entries>& initial) : initial_(initial), current_(initial) {}

  void begin(bool reset) {
    next_ = current_;
    reset_ = reset;
  }

  uint64_t read(uint64_t index) const { return index < Entries ? current_[index] : 0; }

  void write(uint64_t index, uint64_t value) {
    if (index < Entries) next_[index] = value & mask(Width);
  }

  void writeField(uint64_t index, Field field, uint64_t value) {
    assert(field.width > 0 && field.offset + field.width <= Width);
    if (index >= Entries) return;
    const uint64_t field_mask = mask(field.width) << field.offset;
    next_[index] = (next_[index] & ~field_mask) | ((value & mask(field.width)) << field.offset);
  }

  template <typename Predicate>
  std::array<bool, Entries> camHitVec(Predicate predicate) const {
    std::array<bool, Entries> result{};
    for (size_t index = 0; index < Entries; ++index) result[index] = predicate(current_[index]);
    return result;
  }

  template <typename Predicate>
  std::array<uint64_t, Entries> camHitIdx(Predicate predicate) const {
    std::array<uint64_t, Entries> result{};
    size_t count = 0;
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(current_[index])) result[count++] = index;
    }
    while (count < Entries) result[count++] = Entries;
    return result;
  }

  template <size_t MaxHits, typename Predicate>
  std::array<Hit, MaxHits> camRead(Predicate predicate) const {
    static_assert(MaxHits > 0 && MaxHits <= Entries);
    std::array<Hit, MaxHits> result{};
    size_t count = 0;
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(current_[index])) {
        assert(count < MaxHits);
        if (count < MaxHits) result[count++] = {current_[index], true};
      }
    }
    return result;
  }

  template <typename Predicate>
  void camWrite(Predicate predicate, uint64_t value) {
    for (size_t index = 0; index < Entries; ++index) {
      if (predicate(current_[index])) next_[index] = value & mask(Width);
    }
  }

  template <typename Transform>
  void update(Transform transform) {
    for (size_t index = 0; index < Entries; ++index) next_[index] = transform(current_[index]) & mask(Width);
  }

  void commit() {
    if (reset_) current_ = initial_;
    else current_ = next_;
    reset_ = false;
  }

private:
  std::array<uint64_t, Entries> initial_{};
  std::array<uint64_t, Entries> current_{};
  std::array<uint64_t, Entries> next_{};
  bool reset_ = false;
};

uint64_t next_random(std::mt19937_64& rng) { return rng(); }

void require(bool condition, const char* message) {
  if (!condition) {
    std::cerr << "FAIL: " << message << '\n';
    std::exit(1);
  }
}

template <size_t Entries, unsigned Width, size_t PendingWrites>
void check_random() {
  constexpr uint64_t Initial = 0x5a;
  std::array<uint64_t, Entries> initial{};
  for (size_t index = 0; index < Entries; ++index) initial[index] = (Initial + index * 17) & mask(Width);

  storage_boundary::Storage<Entries, Width, PendingWrites> model(initial);
  Reference<Entries, Width> reference(initial);
  std::mt19937_64 rng(0x5eed5eed);

  for (size_t cycle = 0; cycle < 20000; ++cycle) {
    const bool reset = (next_random(rng) % 97) == 0;
    model.begin(reset);
    reference.begin(reset);

    const size_t operations = next_random(rng) % 4;
    for (size_t operation = 0; operation < operations; ++operation) {
      switch (next_random(rng) % 4) {
        case 0: {
          const uint64_t index = next_random(rng) % Entries;
          const uint64_t value = next_random(rng);
          model.write(index, value);
          reference.write(index, value);
          break;
        }
        case 1: {
          const uint64_t index = next_random(rng) % Entries;
          const unsigned offset = static_cast<unsigned>(next_random(rng) % Width);
          const unsigned available = Width - offset;
          const unsigned width = 1 + static_cast<unsigned>(next_random(rng) % available);
          const uint64_t value = next_random(rng);
          model.writeField(index, {offset, width}, value);
          reference.writeField(index, {offset, width}, value);
          break;
        }
        case 2: {
          const uint64_t key = next_random(rng) % (UINT64_C(1) << Width);
          const uint64_t value = next_random(rng);
          const auto predicate = [key](uint64_t entry) { return entry == key; };
          model.camWrite(predicate, value);
          reference.camWrite(predicate, value);
          break;
        }
        default: {
          const uint64_t amount = next_random(rng) % (UINT64_C(1) << Width);
          const auto transform = [amount](uint64_t entry) {
            return (entry + amount) & mask(Width);
          };
          model.update(transform);
          reference.update(transform);
          break;
        }
      }
    }

    for (size_t index = 0; index < Entries; ++index) {
      require(model.read(index) == reference.read(index), "read before commit");
    }

    const uint64_t query = next_random(rng) % (UINT64_C(1) << Width);
    const auto predicate = [query](uint64_t entry) { return entry == query; };
    require(model.camHitVec(predicate) == reference.camHitVec(predicate), "camHitVec");
    require(model.camHitIdx(predicate) == reference.camHitIdx(predicate), "camHitIdx");
    const auto model_hits = model.template camRead<Entries>(predicate);
    const auto reference_hits = reference.template camRead<Entries>(predicate);
    for (size_t index = 0; index < Entries; ++index) {
      require(model_hits[index].value == reference_hits[index].value, "camRead value");
      require(model_hits[index].hit == reference_hits[index].hit, "camRead hit");
    }

    model.commit();
    reference.commit();

    for (size_t index = 0; index < Entries; ++index) {
      require(model.read(index) == reference.read(index), "read after commit");
    }
  }
}

void check_adapter() {
  unsigned long long out = 0;
  unsigned long long hit = 0;
  unsigned long long mask_out = 0;
  std::array<unsigned long long, 4> indices{};
  std::array<unsigned long long, 4> snapshot{};

  storage_prepare(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  storage_read_0(1, 0, &out);
  require(out == 5, "adapter reset entry 0");

  storage_prepare(0, 1, 1, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  storage_read_1(2, 1, &out);
  require(out == 99, "adapter indexed write");

  storage_prepare(0, 0, 0, 0, 1, 0, 4, 4, 0xa, 0, 0, 0, 0, 0);
  storage_read_0(3, 0, &out);
  require(out == 0xa5, "adapter field write");

  storage_prepare(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 20, 0x30, 0, 0);
  storage_read_0(4, 2, &out);
  require(out == 0x30, "adapter CAM write");

  storage_prepare(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1);
  storage_snapshot(5, snapshot.data());
  require(snapshot[0] == 0xa6, "adapter update entry 0");
  require(snapshot[1] == 100, "adapter update entry 1");
  require(snapshot[2] == 0x31, "adapter update entry 2");
  require(snapshot[3] == 8, "adapter update entry 3");

  storage_query_0(6, 0x31, &out, &hit);
  require(out == 0x31 && hit == 1, "adapter camRead");
  storage_query_1(7, 0x31, &mask_out);
  require(mask_out == 4, "adapter camHitVec");
  storage_query_2(8, 0x31, indices.data());
  require(indices[0] == 2 && indices[1] == 4 && indices[2] == 4 && indices[3] == 4,
          "adapter camHitIdx");
}

}  // namespace

int main() {
  check_random<8, 8, 64>();
  check_adapter();
  std::cout << "PASS Storage template and adapter\n";
  return 0;
}
