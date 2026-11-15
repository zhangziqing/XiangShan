# Storage software model: template + few calls

This experiment migrates the Queue boundary scheme from agent
`1d4768d5-f443-4c49-9409-dd622182dd05` to `utils.QueueModel.Storage`:

- **Software function template**: [storage.hpp](storage.hpp) owns the storage
  words, pending writes, reset contents and cycle commit. It exposes one native
  function per Storage semantic.
- **Few calls**: [storage_adapter.cpp](storage_adapter.cpp) is the generated
  adapter shape. It has a fixed call set independent of capacity:
  `prepare`, `read_0`, `read_1`, `query_0`, `query_1`, `query_2`, `snapshot`.
  No per-slot function is emitted.

The Queue reference is `ring_queue.hpp` plus the generated `queue.cpp` from
`experiments/queue-model` in the `wise-baboon` worktree. There, a 72-entry
Freelist generated one `prepare`, one `state` and three `read` queries. Here a
4-entry Storage generates seven boundary calls; a 16- or 72-entry Storage keeps
the same call set because the template loops internally.

## Mapping

| Queue reference | Storage migration |
| --- | --- |
| `RingQueue<Entries, Width, Ports>` | `Storage<Entries, Width, PendingWrites>` |
| `read(index)` | `read(index)` |
| `enq` / `deq` / pointer updates | `write`, `writeField`, `camWrite`, `camWriteField`, `update` |
| `count` / `empty` / `full` | `camHitVec`, `camHitIdx`, `camRead` |
| `begin` / `commit` | `begin` / `commit` |
| `queue_prepare` (one function) | `storage_prepare` (one function) |
| `queue_query_<id>` (one per result call) | `storage_read_<id>` / `storage_query_<id>` |
| `static Queue queue` + epoch `sync` | `static Storage storage` + epoch `sync` |

`writeField` and the field form of `camWrite` accept the lowered form of a
validated `FieldOf` path: `Field{offset, width}`. The Scala/generator side
resolves the descriptor; the software template never re-runs a Scala selector.

## Semantics

- Reads, `camHitVec`, `camHitIdx`, `camRead` and `foreach` observe committed
  state; pending writes are not forwarded.
- `write`, `writeField`, `camWrite`, `camWriteField` and `update` stage writes.
  `commit` applies them in call order, so later enabled writes win per bit.
- `camWrite` predicates read committed state once at call time; a preceding
  write in the same cycle is not visible to the predicate.
- `update` replaces each whole entry from committed state; multiple updates do
  not compose, matching `Storage.update`.
- Reset discards pending writes and restores the initial contents.
- Dynamic out-of-range reads return zero; out-of-range writes do nothing.
- `camHitIdx` returns `Entries` ascending indices padded with `Entries`;
  `IndexWidth = bit_width(Entries)` matches `max(1, ceil(log2(entries + 1)))`.
- `camRead<MaxHits>` returns physical-order `{value, hit}` pairs padded with
  `{0, false}` and asserts on an over-hit, matching the hardware bound.

## Build and run

```bash
cd experiments/storage-model
mkdir -p build
g++ -std=c++17 -O2 -Wall -Wextra -fsanitize=address,undefined \
  check_storage.cpp storage_adapter.cpp -o build/check_storage
./build/check_storage
```

The check compares the template with an independent next-state reference over
20,000 random cycles (writes, field writes, CAM writes, updates and resets),
then drives the adapter through reset, indexed write, field write, CAM write,
update, CAM queries and snapshot.

## Scope and next step

The template represents fixed-width scalar words up to 64 bits. Bundle entries
need a generated packer from `FieldOf` paths and per-leaf widths; the current
`Field` is the scalar lowering. Predicates and transforms are C++ lambdas; a
capture/emitter would generate the call arguments and lambda bodies from the
Chisel Storage call sites, analogous to `boundary_capture.py` and
`boundary_emit.py`. The adapter is a hand-written example of that generated
shape; one adapter represents one hardware instance, so multiple instances need
distinct symbols. A SystemVerilog/DPI shell is not included in this prototype.
