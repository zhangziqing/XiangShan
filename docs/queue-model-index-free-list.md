# IndexFreeList correspondence to FreeList

`utils.QueueModel.IndexFreeList` preserves its constructor and matches the public
IO of the original `xiangshan.mem.FreeList`. The latter remains unchanged as the
independent reference. The implementation uses composition to keep child state
behind static IO while preserving the original allocator's storage and
transition structure.

## Module and abstraction boundaries

- `IndexFreeList` owns the pending-return bitmap, registered residue-class
  selection, registered `freeSlotCnt` observation and performance events.
- Its private `IndexFreeListRing` child extends `ElementQueue[UInt]` with
  `QueueStatus`. It owns identity-initialized entries and the two circular
  endpoints. Element writes compose through `update`; reads use `foreach`.
- The child IO carries requests, accepted allocation bits, registered returns,
  allocation previews and `nextFreeCount`. The wrapper accesses no child register
  or protected pointer. The caller's circuit-selection key reaches the child.
- `QueueStatus` retains its pointer-derived count/empty/full implementation.
  The wrapper's legacy `freeSlotCnt` observation is separate from that common
  abstraction and retains its original reset and next-state arithmetic.

This changes the Module hierarchy, not the allocator's external cycle boundary.
Neither a new queue backend nor host-side execution is introduced.

## State and execution contract

| Original FreeList state | IndexFreeList state | Update |
| --- | --- | --- |
| `freeList(i)` | child entry `i` | Reset to `i`; original sparse return-port order and last enabled write priority. |
| `headPtr` | child `deqPtrReg` | Reset to phase 0/index 0; add `PopCount(doAllocate)` when nonzero. |
| `tailPtr` | child `enqPtrReg` | Reset to phase 1/index 0; add `PopCount(freeReq)` when nonzero. |
| `freeSlotCnt` | wrapper `freeSlotCnt` | Reset to size; register `distanceBetween(tailNext, headNext)` every edge. |
| `freeMask`, selection registers | corresponding wrapper registers | Original bitmap selection and registration stages. |
| preallocation registers | child preview registers | Original unreset `RegEnable` behavior. |
| empty event registers | wrapper event registers | Original two-register event path. |

All arithmetic is unsigned and uses the original `CircularQueuePtr` helpers.
Indices have `log2Up(size)` bits, endpoints add one phase bit, and the observed
count has `log2Ceil(size + 1)` bits. Truncation, overflow, circular ordering and
single-subtraction non-power-of-two wrap follow the reference helpers. In
particular, preallocation uses `(head + allocatedCount) + requestRank`, preserving
the reference's intermediate pointer width and wrap operation.

Combinational previews read old payload/endpoints. There is no return bypass.
A new free pulse passes the pending-mask edge, selection edge and tail/payload
edge before becoming available. Preallocation adds the original candidate
register stage; its unreset outputs are unspecified before a sampling edge.
Only the population count of `doAllocate` matters, including sparse masks.
Requests are not clamped or repaired. Ownership/capacity discipline determines
whether status denotes a legal dense interval, rather than changing transitions.

Active-high reset propagates to the child with the caller's reset type. Both
synchronous and asynchronous variants are checked. Unreset selection, preview
and event state retains the reference semantics during reset and initial cycles.

## Verification interface and scope

`xiangshan.utils.IndexFreeListEc` emits separate actual RTL closures with identical
`FreeListEcTop` wrappers. Each exposes all candidate bits, can-allocate bits,
validCount, empty and the six-bit hardware event, including invalid candidates.
Clock, reset, allocateReq, doAllocate and free remain unconstrained inputs.
The wrapper adds no state, admission logic or output masking.

The checked `(size, allocation width, return width, preallocation)` configurations
are RAR `(72,3,4,false)`, RAW `(32,3,4,true)`, Replay `(72,3,4,true)`,
Uncache `(16,3,4,true)`, VL `(16,2,2,false)`, VS `(16,2,1,false)`,
small `(6,3,2,false/true)` and one-lane `(2,1,1,false/true)`.

All ten configurations pass standalone Formality EC with each reset type:
20 positive runs. Inverting candidate 0 bit 0 in the proof-only Uncache wrapper
fails at exactly that output under each reset type. The production RTL is not
mutated. The two existing allocator suites also pass all 16 tests.

Formality V-2023.12-SP3 uses Equality passing mode, Conservative initialization,
two cores and a 300-second verification limit per case. Each positive run has
complete port/state inventories, no unmatched/failing/aborted/unverified points, no black
boxes, user cuts, disabled points, input constraints or user constants. Every
public output bit is listed as passing. Unread points are lowering temporaries
and structurally zero residue positions; resettable payloads, endpoints, count
and candidate registers remain proved. No DFFX state is present. Constrained 0X
residue bits retain their original unreset-to-zero behavior.

Generation uses Mill 0.12.3, Scala 2.13.15, Chisel 6.7.0 and firtool 1.62.1.
The shared utility revision is `2f0743f1f3ee1889049841926fa382cd0b32d8e2`,
matching the frozen baseline. Proofs cover the listed elaborations, including
arbitrary request/return bit patterns; they do not establish all parameter
values, enclosing-system EC or PPA. Enabled debug-counter printing is not a
separate EC configuration.

Generate with `mill -i -j 2 xiangshan.test.runMain
xiangshan.utils.IndexFreeListEc <new-output-directory>`, adding `--async-reset`
for the asynchronous variant. The hier-model repository's
`scripts/xiangshan/index_freelist_formality.py` prepares explicit inputs and Tcl;
`skills/formality-verification/scripts/run_formality.py` preserves each tool run.
The tracked hier-model task record identifies exact source and raw evidence.
