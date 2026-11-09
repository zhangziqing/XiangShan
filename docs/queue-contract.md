# VirtualLoadQueue contract and model substitution

VirtualLoadQueue owns one RingQueue of entries and two completion mechanisms.
Scalar load writeback addresses a slot and patches completion/debug fields;
vector feedback matches the old entry's ROB/uop identity and vector membership.
Ordered retirement consumes the completed prefix. Redirect invalidation and its
delayed tail rollback remain owner policy.

## Source contract

There is one inherited payload table and one head/tail pair. Production code
reads entries directly; it does not reconstruct parallel field-array views.
`deq(lastCommitCount)` supplies the old head window. Content-based vector
completion and cancellation use `update`; known-slot retirement and scalar
writeback use `writeField`. All predicates/RHS expressions read pre-edge state.
Later enabled writes win independently per field: dispatch initialization,
retirement validity clear, vector completion, redirect validity clear, then
scalar writeback in ascending lane order. The last scalar lane wins collisions.

The range overload `enq(elements, ranges, reserved)` makes the dispatch extension
explicit. Each input supplies a complete payload, absolute circular start and
length. `enqGuard` decides payload acceptance; accepted overlapping ranges use
lowest-port priority. The independent reservation count includes canceled flows.
Recovery replaces tail movement without suppressing range payload writes; flush
suppresses both. The packed enqueue overload keeps its original behavior.
This extension is necessary because canceled dispatch can reserve slots without
initializing payload. Admission assertions, address/flow constraints and the
delayed rollback/head clamp stay in VirtualLoadQueue. SQ is unchanged here.

Only validity and vector membership reset in the entry table. ROB/uop identity,
completion and debug fields keep their original unreset behavior, including
allocation writes during reset. Pointer widths, phase arithmetic, flow-count
truncation and the registered retirement/cancel delays are inherited from the
original typed Chisel expressions; no host-integer modulo substitutes for them.

## Extraction boundary and execution

`QueueContractAnnotation` marks the owning module, queue parameters and state
roots `contents`, `enqPtr`, `deqPtr`; it adds no production ports or wrapper.
CHIRRTL emission writes it to a `*.queue-contract.json` sidecar through Chisel's
CustomFileEmission API. Normal CIRCT/SystemVerilog generation excludes this
metadata from firtool's input and therefore needs no unknown-annotation option.
`scripts/queue_contract.py` combines this annotation with lowered FIRRTL. It
exports an explicit typed transition graph: state next values, clock/reset
conditions and values, observations, assertion predicates, dependency nodes,
source locations and the input FIRRTL SHA-256. Queue roots and required owner
state are labeled separately. Dead owner state is omitted only after dependency
closure over queue state, outputs and assertions.

The module hierarchy remains unchanged. The abstraction boundary in this pilot
is the complete concrete VirtualLoadQueue, including owner delay state. The
execution representation is `contract.json` plus generated simulation
SystemVerilog. This is an elaborated transition contract; it does not retain a
high-level enqueue/update operation IR or demonstrate a queue-only replacement.
The SV emitter explicitly sizes expressions and commits state with nonblocking
assignments. AsyncReset registers also react between clock edges. Unsized host
arithmetic and sequential in-place state mutation are not used for the model.

The extractor rejects unknown statements/primitives, abstract resets, missing
state roots, unresolved dependencies and incomplete/unlowered connections.
Its supported subset has concrete positive-width UInt/SInt, Clock/AsyncReset,
nodes, single connects, registers and assertions. Memories, nested instances,
external modules, multiple clocks with interacting edges and arbitrary FIRRTL
are outside the validated profile. Diagnostic printf text is omitted; assertion
predicates are retained. FIRRTL expression widths/signedness follow the
[official specification](https://github.com/chipsalliance/firrtl-spec/blob/main/spec.md).

The replacement checker requires every model-used input to survive RTL lowering.
Test instances use `dontTouch(io)` to preserve constant input connections across
the substitution boundary. Without this, cross-module constant propagation can
prune a true input and leave a replacement model's input undriven. Unused model
ports may remain unconnected; needed inputs are checked before building.

## Independent reference and observations

`LegacyVirtualLoadQueue.scala` comes from baseline
`e7bab53e66dfb3c4a1d11cf9519b0396f8576cae`, with a class rename and three
width-explicit zero reset arguments to `GatedRegNext`. The latter avoid a
firrtl2 AsyncReset restriction on a wire produced by `asTypeOf`; they preserve
the helper's original zero reset and widths. Its allocation, completion,
retirement and redirect algorithms are independent of RingQueue and unchanged.

The miter compares public outputs, all validity/vector bits and every defined
payload field. Unreset identity is compared after allocation, completion/debug
after allocation or writeback, including after invalidation and subsequent
reset. The initial test-only definedness pulse is separate from DUT reset. This
does not prove four-state equivalence for power-on undefined payload. Verilator
executes both RTL and generated SV as two-state models. The async fixture also
checks a pulse entirely between clock edges and allocation while reset is held.

## Reproduce

Use Python 3, Scala 2.13.15, Chisel 6.7.0, firtool 1.62.1 and Verilator 5.036.
The usual repository dependency checkout/build setup is required. For example:

```sh
mill -i xiangshan.test.testOnly utils.QueueModel.ReservationEnqueueSpec xiangshan.mem.VirtualLoadQueueSpec xiangshan.mem.VLoadQueueContentSpec xiangshan.mem.VLoadQueueTargetedSpec xiangshan.mem.VirtualLoadQueueResetSpec
mill -i xiangshan.test.runMain xiangshan.mem.ContractElab /tmp/vlq-annotations-8 8
mill -i xiangshan.test.runMain xiangshan.mem.ContractElab /tmp/vlq-annotations-72 72
mill -i xiangshan.test.runMain xiangshan.mem.ContractElab /tmp/vlq-annotations-async 8 async
mill -i xiangshan.test.runMain xiangshan.mem.ContractElab /tmp/vlq-normal-rtl 8 sync systemverilog
python3 scripts/test_queue_contract.py -v
```

Chiseltest emits lowered FIRRTL and SV into `test_run_dir/`; its behavioral
backend does not consume the contract metadata. `ContractElab` with the default
CHIRRTL target separately writes the JSON sidecar. Use the same compiled
sources/configuration for both.
For the depth-8 content test, run the following from the repository root;
`/tmp/vlq-model-8` must not already exist:

```sh
python3 scripts/check_vlq_contract.py \
  test_run_dir/should_preserve_oldstate_vector_matching_multilane_allocation_and_later_writeback/VLoadQueueMiterHarness.lo.fir \
  /tmp/vlq-annotations-8/VLoadQueueMiterHarness.TestableVirtualLoadQueue.queue-contract.json \
  test_run_dir/should_preserve_oldstate_vector_matching_multilane_allocation_and_later_writeback/VLoadQueueMiterHarness.sv \
  /tmp/vlq-model-8 --scalar-type 32768 --vector-type 2147483648
```

For depth 72 use the test directory
`should_cover_nonpoweroftwo_72_wrap_rollback_and_redirects`, the depth-72
annotations and a fresh output directory. For asynchronous reset use
`should_match_baseline_asynchronous_metadata_reset_and_unreset_payload_writes`,
top files `VLoadQueueResetHarness.{lo.fir,sv}`, sidecar
`VLoadQueueResetHarness.TestableVirtualLoadQueue.queue-contract.json` and
`--async-reset`. FuType arguments are the scalar `ldu` and vector `vldu` one-hot
values in this revision; update them if the source enumeration changes.

The script writes the extracted contract, standalone `queue-model.sv`, unchanged
RTL miter, substituted miter, deterministic driver, build/run logs and JSON
results. It runs the same independent reference before and after substitution,
then injects a wrong completion transition (wrong vector flag for the async
fixture) and requires the comparator to fail.

## Evidence and limits

Behavior verification and extraction verification are separate:

| Check | Coverage |
| --- | --- |
| Independent RTL reference | Seven VLQ tests: scalar/reset/retirement, three-port/vector old-state collisions, variable-flow allocation, canceled reservations, 1,200-cycle depth-72 wrap/redirect traffic, between-edge asynchronous reset |
| Shared range enqueue | Four tests at depths 1, 6, 8, 72: canceled/no-write reservation, overlap/wrap, old-state update priority, recovery, guards, flush and reset |
| Extracted model substitution | Depths 8 and 72: each 5,280 cycles, 840 allocation offers, 240 vector offers, 240 redirects, eight resets; faulty transition must be detected |
| Async model substitution | Between-edge pulse and held reset with unreset payload writes; faulty vector-flag transition must be detected |
| Extractor checks | Missing roots/connections, unsupported effects/primitives/types and 64 signed input pairs with old-state commits |

The local verification uses fresh changed production/test classes before warmed
dependency classes, with commands and hashes recorded in the hier-model task
evidence. Full repository build, Formality/EC, full-system equivalence, other
configurations, a semantic C++ backend and performance/resource improvement are
not established. Generated SV still evaluates a lowered expression graph; this
pilot supplies a checked extraction/substitution path, not an acceleration claim.
