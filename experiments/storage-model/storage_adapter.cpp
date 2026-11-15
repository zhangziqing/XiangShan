// Example generated adapter for one Storage instance.
//
// The adapter has a fixed, small call set independent of capacity:
//   prepare, read_0, read_1, query_0, query_1, query_2, snapshot.
// Each call forwards to the Storage template; no per-slot function is emitted.
#include "storage.hpp"

namespace {

constexpr size_t Entries = 4;
constexpr unsigned Width = 8;
constexpr size_t PendingWrites = 16;

using Storage = storage_boundary::Storage<Entries, Width, PendingWrites>;

// One generated adapter represents one hardware instance. Additional instances
// need separately generated symbols, as in the Queue boundary model.
Storage storage{{5, 10, 20, 7}};
unsigned long long committed_epoch = 0;

Storage& sync(unsigned long long epoch) {
  if (committed_epoch != epoch) {
    storage.commit();
    committed_epoch = epoch;
  }
  return storage;
}

}  // namespace

extern "C" void storage_prepare(
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
    unsigned long long update_amount) {
  auto& model = storage;
  model.begin((reset & 1) != 0);
  if ((write_enable & 1) != 0) {
    model.write(write_index, write_value);
  }
  if ((field_enable & 1) != 0) {
    model.writeField(
        field_index,
        {static_cast<unsigned>(field_offset), static_cast<unsigned>(field_width)},
        field_value);
  }
  if ((cam_enable & 1) != 0) {
    model.camWrite([key = cam_key](uint64_t value) { return value == key; }, cam_value);
  }
  if ((update_enable & 1) != 0) {
    model.update([amount = update_amount](uint64_t value) {
      return (value + amount) & storage_boundary::mask(Width);
    });
  }
}

extern "C" void storage_read_0(
    unsigned long long epoch, unsigned long long index, unsigned long long* out) {
  *out = sync(epoch).read(index);
}

extern "C" void storage_read_1(
    unsigned long long epoch, unsigned long long index, unsigned long long* out) {
  *out = sync(epoch).read(index);
}

extern "C" void storage_query_0(
    unsigned long long epoch, unsigned long long key,
    unsigned long long* out_value, unsigned long long* out_hit) {
  const auto hits = sync(epoch).camRead<2>(
      [key](uint64_t value) { return value == key; });
  *out_value = hits[0].value;
  *out_hit = hits[0].hit ? 1 : 0;
}

extern "C" void storage_query_1(
    unsigned long long epoch, unsigned long long key, unsigned long long* out_mask) {
  const auto hits = sync(epoch).camHitVec(
      [key](uint64_t value) { return value == key; });
  uint64_t mask = 0;
  for (size_t index = 0; index < Entries; ++index) {
    if (hits[index]) mask |= UINT64_C(1) << index;
  }
  *out_mask = mask;
}

extern "C" void storage_query_2(
    unsigned long long epoch, unsigned long long key, unsigned long long* out_indices) {
  const auto indices = sync(epoch).camHitIdx(
      [key](uint64_t value) { return value == key; });
  for (size_t index = 0; index < Entries; ++index) out_indices[index] = indices[index];
}

extern "C" void storage_snapshot(unsigned long long epoch, unsigned long long* out) {
  const auto& model = sync(epoch);
  for (size_t index = 0; index < Entries; ++index) out[index] = model.read(index);
}
