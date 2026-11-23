# Complete LoadQueueRAR Storage/CoSim validation

## Production model boundary (2026-11-23)

This change moves the complete RAR entry and its predicates into generated C++.
The default 72-entry, three-lane module passes the original RTL's two 65,536-cycle
traces with both Storage backends. Ten fixed-core, alternating throughput pairs
measure a median **1.485x model/RTL speedup**. Retired instructions fall **2.65%**
and conditional branches fall **55.51%** per simulated hardware cycle.

These are standalone complete-module measurements, including the internal
FreeList and the RTL controller. They do not measure a standalone FreeList,
a storage array alone, a whole SoC, or a wholly C++ RAR controller.

### Representation and interface contract

The source change starts from accepted base `ec165efa7a`. It keeps the accepted
consolidated Storage files and generation-wide RTL/CoSim selection. Supporting
pure callback capture, named C++ emission and the shared Bits runtime are included
because those dependencies were not present on that base.

| Storage | Entries x logical bits | Fields and reset coverage |
| --- | --- | --- |
| RAR | 72 x 35 | `allocated`, `released` reset to false; 16-bit address hash, 9-bit ROB and 8-bit LQ pointers remain unreset |
| FreeList | 72 x 7 | Slot indices, initialized to 0 through 71 |

All RAR table data stays in the selected Storage backend. There is no identity
`foreach` snapshot, separate RTL address table, or separate RTL release table.
The sealed RAR descriptor contains three enqueue writes, two physical-mask field
writes, three revoke field writes, five queries, and six indexed reads. The
queries return two 72-bit masks and three scalar violation results. Indexed reads
select the enqueue/revoke slots for ownership checks. The FreeList slot table
also uses the selected backend.

Queue pointers, allocation policy, handshakes, registered response bits and the
sampled physical release mask remain RTL control/pipeline state. The entry
hierarchy, controller hierarchy and C++ execution representation are distinct:
combining entry fields does not replace the complete controller with C++.

The new generic mechanisms are:

- Per-session `FunctionId` values span captured Chisel modules, saved CHIRRTL and
  lowered FIRRTL, JSON, generated SV bridges and named `intention_function_<id>`
  C++ functions. Concrete Data classes and their methods survive invocation;
  the final descriptor contains compiler IR and metadata rather than closures.
- Pure, combinational callbacks support `update`, `foreach` and the CAM APIs.
  Hardware context is passed as explicit typed parameters. External hardware
  references, stateful bodies and unsupported primitives fail generation.
  Entry, input parameter and callback output widths are at most 64 bits; the
  Bits runtime supports intermediates through 128 bits.
- `camAny` returns a scalar reduction. Its branchless C++ reduction permits
  vectorization. `camAny` and `camHitVec` accept an explicit enable and return
  false/zero when disabled; default enable preserves existing query behavior.
- Query results cross both the DPI ABI and the blackbox boundary as packed
  64-bit words. CAM masks use byte-group packing with a scalar tail/fallback.
  Field writes selected by physical masks iterate set bits rather than emitting
  one write request for every slot.
- Schema 3 records partial reset masks, physical mask writes and explicit query
  enables. Existing executable schema 1/2 descriptors remain readable. Partial
  aggregate literals are inspected through a small Chisel 6.7 binding adapter;
  arbitrary hardware initializers are rejected.

### Cycle correspondence

Reads and callback predicates observe committed old state without forwarding.
Enqueue writes occur first, reclamation and revoke clear ownership afterward,
and the delayed physical-slot release write occurs last. Partial reset applies
only to the resettable fields after those writes, so normal writes to unreset
payload still occur during reset. Full-entry reset keeps its existing priority.

The release query samples the old allocation/address match into a physical mask.
The next cycle updates those same slots even if a slot was freed or reused. It
does not repeat an address lookup against the next cycle's entries. The original
`LqPAddrModule(numWDelay = 1)` address write edge is preserved.

The violation result is `RegEnable(camAny(...), req.valid)` with the original
`RegNext(req.valid)`. Moving OR before the common register enable preserves
latency and the response value held during invalid cycles. Address hashing,
wrap-aware pointer comparisons, redirects, enqueue readiness and FreeList
membership policy retain their source behavior.

This is a two-state, clock-sampled reset model. Initialization correspondence
starts after reset reaches the storage clock. Asynchronous pulses between clock
edges and four-state/X correspondence remain outside the existing model contract.
These trace tests provide cycle comparison for the recorded configuration and
inputs; they are not a formal all-input equivalence proof.

### Verification

All Scala verification sources and generated artifacts remain local and ignored.
No Scala tests/specs are included in the PR.

| Executed check | Result |
| --- | --- |
| `mill -i xiangshan.compile` | PASS; final log `mill-compile-release.log` |
| Final Storage RTL vs original RTL, two traces | PASS; 65,532 checked cycles per trace, before and after each edge |
| Final CoSim vs original RTL, same two traces | PASS; same cycle checks, including invalid response hold |
| UInt/SInt 1/8/63/64-bit and Bundle/Vec callback matrix | PASS; 9 models x 5,000 cycles in each indexed-read bridge mode |
| All callback APIs and mixed write ordering | PASS; 50,000 cycles against independent RegStorage implementation |
| 65-entry, 64-bit entry boundary miter | PASS; 12,000 cycles, partial reset writes, disabled queries, masks crossing 63/64/65, invalid indices and old-state sampling |
| Packed CAM masks | PASS; 1/7/8/9/63/64/65/72 entries, all 256 byte patterns, random masks and padding, ASan/UBSan |
| Bits arithmetic/runtime reference checks | PASS; boost::multiprecision::cpp_int reference with ASan/UBSan, leak detection disabled for sandbox compatibility |
| Historical schema 1/2 JSON and C++ regeneration | PASS; 3 descriptors decode, roundtrip, validate and regenerate |
| Negative capture/initializer checks | PASS; 7 rejected cases, followed by successful generation with both ID spaces restarted |

The two golden digests are `5318ecdff585f57f` and `2dcd7f60c59d7900`.
Each 8-repeat timed run produces `5298129fde605e67`. The first four cycles of
each trace exclude unspecified initial pipeline state. The extra trace covers
active reset with redirect/enqueue activity.

### Throughput and the two expected benefits

The configuration and harness match the historical experiment below: RAR 72,
virtual LQ 72, ROB 160, 3 load lanes, 48-bit physical addresses, 16-bit address
hash, FreeList free width 4, no preallocation, and identical live functional IO.
The common harness calls `eval()` three times per hardware cycle. Construction,
file IO, checking and two warmup repetitions are excluded from the timed region.
Each timed run replays the same 65,536-cycle trace eight times: 524,288 cycles.

Verilator 5.052 uses `--threads 1 -O3 --assert`; clang++ 19.1.1 uses C++17,
`-O3 -DNDEBUG`, with `OPT_FAST`, `OPT_SLOW`, and `OPT_GLOBAL` all `-O3`.
Compiler caching is disabled. Scala 2.13.15, Chisel 6.7.0, firrtl2 6.0,
Chisel's firtool 1.62.1, Mill 0.12.3 and Java 21 are unchanged.

The ten alternating throughput pairs use CPU 56 on an AMD EPYC 9684X; its SMT
sibling is CPU 248. The shared host had substantial competing load. No samples
are silently removed. One slow RTL sample broadens the maximum paired ratio;
this maximum should not be interpreted as an additional algorithmic gain.

| Metric | Original RTL | New CoSim |
| --- | ---: | ---: |
| Median throughput, Mcycle/s | 0.46010 | 0.69312 |
| Throughput range, Mcycle/s | 0.30777-0.47825 | 0.67462-0.70788 |
| Median ns/hardware cycle | 2173.43 | 1442.88 |
| ns/hardware cycle range | 2090.96-3249.18 | 1412.67-1482.32 |
| Runtime peak RSS, first five uninstrumented runs | 6 MiB | 6 MiB |

Median paired speedup is **1.4849x**, range **1.4632-2.3000x** over 10 pairs.
The first five pairs give 1.4816x; the five subsequent pairs give 1.4921x.
Three additional immediately adjacent counter pairs give 1.4904x, range
1.4768-1.5071x. These results establish the measured configuration on this host;
the absolute rates and high outlier ratio should not be generalized to an idle
machine or another workload.

The counters are enabled only around the timed replay through perf control
FIFOs. Small event groups avoid PMU capacity limits. One multiplexed counter
attempt was rejected and retained separately; accepted samples ran at 100%.

| Metric per hardware cycle (median) | Original RTL | Previous packed snapshot CoSim | New CoSim |
| --- | ---: | ---: | ---: |
| Retired instructions | 17090.80 | 27515.56 | 16638.55 |
| Retired branches | 1215.47 | 948.65 | 663.26 |
| Retired conditional branches | 1018.03 | 667.25 | 452.92 |
| Branch misses | 10.10 | 5.56 | 5.55 |
| Frontend-empty dispatch-slot fraction | 44.51% | 23.84% | 20.18% |

The frontend metric is empty slots divided by `6 * ls_not_halted_cyc`, rather
than a fraction of stalled cycles. Its new-model range is 13.12-20.66%; the
corresponding RTL range is 44.43-44.68%. Branch and instruction counts are stable
across the measured rounds. Counter-group elapsed times varied with host load,
so timing is reported from the alternating uninstrumented pairs rather than
mixing elapsed samples from different counter groups.

The final immediately adjacent retired-counter pairs give CPU cycles of
6095.25 -> 4078.72 and IPC of 2.804 -> 4.079. Thus:

- Branch/frontend benefit is substantial: 45.43% fewer total branches and
  55.51% fewer conditional branches, with fewer frontend-empty slots. Smaller
  generated code and vectorization can also contribute; these counters do not
  attribute the entire utilization improvement exclusively to branches.
- Total dynamic CPU work also decreases, but modestly: instructions fall 2.65%
  relative to original RTL and 39.53% relative to the previous packed snapshot
  model. The instruction reduction alone implies about a 1.027x factor. Most
  measured acceleration comes from improved CPU utilization. This does not
  establish a large reduction in the number of algorithmic comparisons.

Removing the snapshot boundary is necessary for these changes: full predicates
and scalar reductions now execute together in the model, inactive queries avoid
scans, a sampled mask drives field updates without another predicate scan, and
packed results avoid per-entry transport/reconstruction. Callback field decoding,
remaining scans, two-version state copies, bridge scheduling and RTL control
still consume work.

### Build cost and raw artifacts

| Verilation + C++ build observation | Original RTL | New CoSim |
| --- | ---: | ---: |
| Wall time | 4.534 s | 7.777 s |
| Build peak RSS | 192 MiB | 189 MiB |
| Generated Verilator C++ bytes | 2478881 | 927763 |
| Binary bytes | 522312 | 459064 |

These builds happened at different times on the shared host. They record cost
and output size, not a paired compilation-speed experiment. Model generation
also performs callback lowering; it is outside the runtime timing region.

Current absolute artifact root:
`/nfs/home/zhangziqing/hier-model/rar-model-pr/build/rar-boundary/`

- `correctness-final.json`: all final complete-RAR cycle comparisons.
- `rtl-final/`, `packed-final/`: generated SV, descriptors, C++, common-harness
  binaries, build commands, source hashes, logs and process resource records.
- `measurement-final/combined-summary.json`: 10-pair throughput, adjacent
  retired-counter summary and percentage reductions.
- `measurement-final/{environment,measurements,postcheck,summary}.json`,
  `runs/`, `postcheck/`, `rejected.json`: every timing/counter sample, CPU
  utilization, exact commands, hardware/tool metadata and the rejected attempt.
- `measurement-final/{bench.cpp,measure.py,postcheck.py,source.patch}`: shared
  counter-control harness, replay commands and measured source delta.
- `boundary-packed/check.log`, `callback-packed/check.log`,
  `matrix-final/check.log`, `matrix-read-final/check.log`,
  `negative-checks.log`, `legacy-codec-checks.log`: verification results and local verification sources.
- `mill-compile-release.log`: successful full project compile. Duplicated Mill
  cache and obsolete intermediate object directories were removed after an
  account space limit interrupted setup; final binaries and raw logs remain.

The immutable original traces, RTL source snapshot and baseline binaries remain
under `/nfs/home/zhangziqing/hier-model/XiangShan/build/loadqueuerar-cosim-20261123/`.
Use `rtl-final/obj/VRarBench` or `packed-final/obj/VRarBench` with a golden trace
and `check` to repeat cycle checks; use `measurement-final/VRarBench` with the
saved scripts for timed replay. Rebuilding Scala requires the normal project
compile and initialized dependencies; cached Scala classpaths are local build
conveniences. No slides were changed.

## Historical experiment and diagnosis

The following results describe the earlier experimental snapshot migration,
before the production boundary change above. Its claims that production RAR
was unchanged apply to that historical run.

The complete standalone LoadQueueRAR runs through the generated Storage/CoSim
backend and matches production RTL on the two traces described below. At the
measured 72-entry, three-lane configuration, CoSim is slower: median throughput
is **0.3508 Mcycle/s**, versus **0.6099 Mcycle/s** for production RTL.
The median paired CoSim/RTL throughput ratio is **0.5713x**.
This is an end-to-end hybrid RTL/C++ module measurement; its control remains in
Verilator. It does not establish performance for a wholly C++ RAR controller.

### Compared implementations and boundary

The baseline is the unmodified production
[LoadQueueRAR.scala](../xiangshan/mem/lsqueue/LoadQueueRAR.scala) at
`85ca8d9e4e4cd4b4a6f463c5fb63ffec3b635df7`, including its address table,
allocation/reclamation control, query pipelines and internal FreeList.
The current production FreeList uses `Storage` with the RTL backend; this is
not a historical pre-Storage source comparison.

Production RAR's own tables have not been migrated to Storage. An experimental
copy, `build/loadqueuerar-cosim-20261123/src/StorageLoadQueueRAR.scala`, routes
those tables through the existing backend. Production Scala sources were not
edited. Both a register-backed version and a CoSim version of this copy were
compared with the original module.

| Generated storage | Entries x bits | Contents and reset contract |
| --- | --- | --- |
| 0 | 72 x 33 | 16-bit partial physical address, 9-bit RobPtr, 8-bit LqPtr; originally uninitialized |
| 1 | 72 x 2 | `allocated`, `released`; both reset to false |
| 2 | 72 x 7 | Internal FreeList slot table; reset to 0 through 71 |

Only `robIdx` and `lqIdx` of stored `DynInst` are consumed by RAR. Keeping that
projection preserves the live data contract and stays within ModelStorage's
64-bit entry limit. The address hash and those pointers share the allocation
write and clock edge (`LqPAddrModule` uses `numWDelay = 1`), so they are grouped.
Flags remain separate to preserve their different reset coverage. Partial flag
writes retain source order: enqueue, ordinary deallocation, revoke, then delayed
release. Membership/pipeline registers, circular-pointer control, pending-free
logic and handshakes remain RTL. There is no per-table RTL fallback in CoSim.

The generated backend contains six named captured functions: two `foreach`
snapshot functions, three ROB/address/TSO CAM predicates and one release-address
CAM predicate. FreeList uses the per-storage indexed-read bridge. This is not an
isolated FreeList, storage-array-only, or hand-written queue benchmark.

One elaboration issue required an explicit width in the experimental copy:
`FreeList.io.allocateSlot` is inferred later by FIRRTL, but ModelStorage captures
indices during elaboration. A 7-bit wire gives the captured index its final width.
Without the field projection and this width, selecting the backend on the
original source would not produce the intended full RAR data model.

### Configuration and harness

- Parameters: `DefaultConfig` with `XSCoreParameters()` for the standalone core;
  RAR 72, virtual LQ 72, ROB 160, three load lanes, eight address write banks,
  48-bit physical addresses, FreeList free width four, no preallocation.
- Synchronous Bool reset; debug/performance printing and performance counters
  disabled equally. Hardware assertions enabled with Verilator `--assert`.
- The same `RarBench` wrapper and `src/bench.cpp` drive every implementation.
  Every RAR input that affects functional behavior is exposed: query valid,
  address, ROB/LQ pointers, data-valid, NC, revoke, release, writeback pointer,
  redirect ROB/level/valid, and TSO. Unused bundle members are tied to zero in
  both implementations. Observed outputs are all three ready, response valid,
  response violation bits, and `lqFull`, including response hold while invalid.
- Each cycle drives inputs and evaluates with clock low, evaluates the rising
  edge, then evaluates the falling edge: three `eval()` calls with identical
  time advancement and output digest work. Timing includes harness execution;
  file loading, golden checking, warmup and model construction are excluded.
- Verilator 5.052, `--threads 1 -O3 --assert`; clang++ 19.1.1, C++17, `-O3
  -DNDEBUG`, and `OPT_FAST`, `OPT_SLOW`, `OPT_GLOBAL` all `-O3` for both builds.
  No compiler cache or waveform/coverage recording. Full commands and source
  hashes are in each implementation's `build.json`.
- Mill 0.12.3, Scala 2.13.15, Chisel 6.7.0, firrtl2 6.0.0, Java 21.
  Generated RTL headers confirm **firtool 1.62.1**. The unrelated `firtool` on
  shell PATH reports LLVM 22 and was not the generator used for these artifacts.
- AMD EPYC 9684X; both measured executables pinned to CPU 81. SMT sibling 273
  was 0–2.4% busy over individual runs. Boost and `schedutil` remained enabled
  on this shared host; the CPU was not reserved exclusively.

### Correctness evidence

The fixed synthetic performance trace has 65,536 cycles, seed
`0x52415220261123`. It includes shuffled full fills, address release, TSO on/off,
revoke, redirect, concurrent retirement/enqueue, non-power-of-two ROB/LQ pointer
wrap and reset. Production RTL records golden outputs before and after every
clock edge. Both experimental implementations match at both samples for all
65,532 checked cycles. The initial four cycles of unspecified pipeline state are
excluded. Golden replay digest: `5318ecdff585f57f`.

Observed performance-trace coverage includes 6,329 full cycles, 199 blocked
query lanes, 50,738 valid violation responses and 35,192 valid non-violation
responses. These are synthetic stimulus/response counts, not application IPC.

A second 65,536-cycle trace adds 320 reset cycles while the queue is active and
64 redirect cycles coincident with enqueue traffic. Original RTL, Storage RTL
and CoSim again match at both samples on all 65,532 checked cycles; digest
`2dcd7f60c59d7900`. Across both traces each implementation has 131,064 checked
cycles, or 262,128 output sample comparisons. No hardware assertion fired.
`mill -i xiangshan.compile` and local Scala harness compilation passed.
Four-state simulation, formal equivalence and full-CPU execution were not run.

### Paired runtime results

Seven rounds alternate RTL/CoSim then CoSim/RTL. Each process first checks one
trace, warms up with two traces, and times eight repetitions: **524,288 cycles
per measured run**, **3,670,016 timed cycles per implementation** in total.
Every timed run has the same digest, `5298129fde605e67`.

| Implementation | Median Mcycle/s | Mcycle/s range | Median ns/cycle | ns/cycle range |
| --- | ---: | ---: | ---: | ---: |
| Production RTL | 0.609877 | 0.598022–0.616983 | 1,639.68 | 1,620.79–1,672.18 |
| Generated Storage/CoSim | 0.350836 | 0.341466–0.351412 | 2,850.33 | 2,845.66–2,928.55 |

The per-round throughput ratios have median **0.571290x** and range
**0.553905–0.587624x**. Dividing the throughput medians gives **0.575258x**;
the corresponding ratio of median time per cycle is **1.73835x**.
The result supports complete-module functional integration, with a runtime
regression at this backend boundary and workload.

A pilot on CPU 79 found its SMT sibling continuously busy. Its seven paired
rounds are preserved in `pilot-cpu79/`, but are excluded from the primary table.
The idle-core rerun above was selected using CPU/sibling utilization, before
examining its performance result.

### Generation, compilation and resources

These are single observed builds, separate from runtime repetitions. Generation
reuses the existing compiled project classes and includes JVM elaboration,
FIRRTL processing, firtool and artifact emission. Verilator/C++ builds use four
build jobs and no compiler cache. They are not repeated compile benchmarks.

| Implementation | Generation wall s | Verilator/C++ build wall s | Build peak RSS KiB | Executable bytes |
| --- | ---: | ---: | ---: | ---: |
| Production RTL | 10.251 | 4.534 | 196,608 | 522,312 |
| Storage RTL correctness comparator | 8.562 | 4.489 | 199,680 | 524,952 |
| Storage/CoSim | 7.951 | 4.426 | 196,268 | 621,440 |

Measured process peak RSS was 6,144 KiB for both runtime variants. JVM generation
peak RSS and complete process statistics are in `resources.json` and the `.time`
files; harness Scala compilation is outside the build-cost table.

The descriptors expose 153 ordered field-write sites for the two flag bits,
in addition to the payload/FreeList writes and bulk-query outputs. The follow-up
below measures the boundary cost instead of attributing the regression from
source-level operation counts alone.

### Regression diagnosis

A follow-up keeps the original binaries and primary measurements intact. User
CPU-cycle sampling with `perf record -e cycles:u -F 1999` profiles the unmodified
executables replaying the same trace (24 timed repetitions, plus normal checks
and warmup). Percentages cover the sampled process, including checks and warmup;
profiled wall times are not used as new primary performance results.

| CoSim sampled work | Share of user CPU cycles |
| --- | ---: |
| Verilator-generated HDL evaluation, including boundary packing/unpacking | 51.22% |
| DPI argument/result conversion wrappers, excluding callees | 21.45% |
| Generated C++ bulk-query bodies, including result stores | 16.60% |
| Verilator trigger/scheduling functions | 4.99% |
| Generated C++ storage commit bodies | 1.77% |
| Generated C++ indexed reads | 0.16% |
| Harness, simulator, libraries and remaining work | 3.82% |

The implementation exports each of the 72 CAM hit bits as a separate 64-bit
output argument. Verilator's DPI wrappers create conversion temporaries and copy
results back into HDL signals. The two `foreach` snapshots likewise export 72
scalar results each. This is an interface representation cost, not 72 necessary
64-bit results for a 72-bit hit mask.

A separate counter-instrumented executable, checked against the original trace,
counts only one replay after warmup. It observes **12.263 DPI calls per hardware
cycle**, including **9.999 bulk queries**: each of four CAMs runs about twice per
cycle, both table snapshots once, plus indexed reads and active commit calls.
This returns about **720 separate 64-bit query values per hardware cycle**
(5,759 nominal bytes before conversion copies). This is scalar ABI volume, not
a measurement of DRAM bandwidth. CAM scans touch every entry on each invocation.
The flags and payload snapshots also return table contents to RTL, where
per-entry retirement/revoke logic and the final hit masking/reduction still run.

The two snapshot queries have consecutive identical inputs on 62.17% and 39.83%
of calls; the four CAMs on 31.49–34.08%. These counters reveal repeated work, but
are not permission to skip edge publication or current-state evaluation.
Any caching or gating change needs the existing edge-ordering checks.

To test the interface hypothesis, an isolated generated-artifact variant changes
only the result ABI of the four 72-bit CAM masks and the 144-bit flag snapshot:
72 scalar output arguments become two 64-bit words per CAM or three words for
the flags. Predicate functions, `Bits` runtime, storage state, commit rules,
payload snapshot, public HDL ports and controller are unchanged. This local
prototype is not a production emitter change.

Five rounds on CPU 81 alternate the four variants in forward/reverse order,
using the original flags, harness and eight trace repetitions per timed run.
SMT sibling activity remains 0–3.3% over individual processes. Results are:

| Variant | Median ns/cycle | Median Mcycle/s |
| --- | ---: | ---: |
| Production RTL | 1,636.98 | 0.610883 |
| Experimental Storage RTL | 1,703.38 | 0.587068 |
| Original generated CoSim | 2,869.48 | 0.348495 |
| CoSim with packed query results | 1,929.70 | 0.518214 |

Packed/original CoSim throughput improves **1.4797x** at the median paired ratio
(range 1.4763–1.4942x). The change removes **76.25% of the additional latency
relative to RTL**, calculated from the medians in this diagnostic comparison.
It still reaches only 0.8483x RTL throughput. Both original correctness traces
pass for the packed variant at both sample points, and all 20 timed runs have
the original matching digest. This identifies result marshalling as the main
measured regression source; the small C++ commit-body share does not identify
153 writes as the principal hotspot. HDL-side write packing remains included
in HDL evaluation and has not been isolated separately.

The remaining cost includes controller work, full-table snapshots, repeated
queries and boundary computation. The Storage RTL comparator also has a modest
code-generation difference from production RTL. These measurements do not give
an exclusive breakdown of that remaining gap or show zero cost for `Bits`;
they show that the largest demonstrated improvement leaves `Bits` unchanged.
A next implementation step should pack the generated query bridge, followed by
reducing table snapshots and moving the complete predicate/reduction into the
software boundary where cycle semantics permit it.

Raw diagnosis is under `build/loadqueuerar-cosim-20261123/diagnosis/`:
`summary.json`, `profile-summary.json`, `*.perf.data`, `*.report.txt`,
`count/counts.jsonl`, `packed/packing.json`, `comparison.json` and
`comparison-summary.json`. `prepare.py`, `build.py`, `profile.py` and `compare.py`
record the isolated instrumentation, ABI prototype, compilation and replay.

### Expected benefits: branch pressure and executed work

The remaining performance question has two separate premises: fewer branches
should reduce frontend pressure, and the model should execute less total work.
A follow-up measures both rather than inferring them from elapsed time or hot
function percentages. The results support the first benefit, while the second
has not been achieved by this implementation.

The original model/runtime object files are relinked against one common local
harness. Its only addition enables perf counters after golden checking and two
warmup traces, then disables them immediately after the timed replay. There is
no instrumentation inside the replay loop. Each binary passes its golden check.
Three rounds alternate forward/reverse implementation order, with eight trace
repetitions (524,288 hardware cycles) in each timed region. All 24 counter runs
have the same passing replay digest. Both threads of CPU 83's core were idle
when selected; SMT sibling utilization stays below 2.9% over the runs.

Two groups of four user-only PMU events run without multiplexing (100% running):
cycles/instructions/branches/branch misses, and cycles/conditional branches/
frontend-empty dispatch slots/not-halted cycles. A six-event probe could not
be scheduled, so it is excluded. The host perf definitions specify
`frontend_bound = de_no_dispatch_per_slot.no_ops_from_frontend /
(6 * ls_not_halted_cyc)`. This is a dispatch-slot fraction, not a percentage of
CPU cycles stalled, and is not an instruction-cache-miss measurement.

| Median metric | Production RTL | Storage RTL | Original CoSim | Packed CoSim |
| --- | ---: | ---: | ---: | ---: |
| Retired instructions / hardware cycle | 17,090.81 | 19,155.67 | 34,580.32 | 27,515.57 |
| Retired branches / hardware cycle | 1,215.47 | 1,052.04 | 968.66 | 948.65 |
| Retired conditional branches / hardware cycle | 1,018.04 | 863.50 | 667.25 | 667.25 |
| Branch mispredictions / hardware cycle | 9.94 | 10.40 | 5.51 | 5.42 |
| Frontend-empty dispatch slots / total slots | 44.68% | 41.65% | 21.45% | 23.55% |
| Retired instructions / CPU cycle (IPC) | 2.875 | 3.025 | 3.350 | 3.934 |

Packed CoSim has 21.95% fewer total branches, 34.46% fewer conditional branches,
and 45.48% fewer branch mispredictions per hardware cycle than production RTL.
Its frontend-empty dispatch slots also fall, both as a fraction and in absolute
slots per hardware cycle (15,936.86 to 9,942.98). These observations support the
branch/frontend benefit. They do not isolate every frontend improvement as a
branch-only effect: code layout, vectorization and other execution changes can
also contribute.

However, retired instructions rise **61.00%**, while IPC improves **36.82%**.
Using `CPU cycles = retired instructions / IPC`, the measured ratios give
`1.6100 / 1.3682 = 1.1767`: about 17.7% more CPU cycles per hardware cycle.
The first improvement is outweighed by the additional executed work. Retired
instructions include loads, stores, conversions and scheduling as well as
arithmetic, so they measure CPU work rather than an isolated arithmetic count.
Even the Storage RTL comparator executes about 12.1% more instructions than
production RTL; the extra work is not exclusively a DPI call cost.

The current lowering explains why a reduction in total work is not automatic:

- `payload.foreach(identity)` and `flags.foreach(identity)` export the whole
  table back to RTL. The original per-entry deallocation, redirect, revoke and
  final hit-filtering/reduction dataflow is retained around these snapshots.
- Each software CAM still scans all 72 payload entries. Its predicate covers
  the address/ROB comparison, while `allocated` and `released` are combined
  afterward in RTL. A compact C++ loop reduces code expansion but does not by
  itself reduce the number of entries compared; the measured queries also run
  about twice per hardware cycle.
- The packed prototype first produces the existing 72-element result array,
  packs its bits, and then exposes individual HDL outputs again. It removes
  excessive scalar ABI arguments but does not fuse the predicate, filtering,
  reduction and response pipeline into one software operation. Packing and
  extracting fields, snapshots, state publication and bridge scheduling remain.

These are concrete additional or retained paths, not an exclusive attribution
of all 10,424.76 extra instructions. An optimization that realizes the second
premise must eliminate work: for example, return the final query reduction when
only that result is observable, avoid rebuilding table snapshots in RTL, and
avoid repeated scans where inputs/state have not changed. Such transformations
must preserve old-state reads, write priority, response latency and invalid-cycle
hold behavior. Simply removing more branches or shortening a generated source
file is insufficient acceptance evidence; instructions and CPU cycles per
hardware cycle must also be measured.

Raw counter evidence and the counter-control harness are in
`build/loadqueuerar-cosim-20261123/diagnosis/benefit/`: `summary.json`,
`measurements.json`, `measurements/*.stat`, `build.json`, `bench.cpp`, `build.py`
and `measure.py`. Event definitions and core selection are recorded in the
parent diagnosis directory. The original binaries and primary timing results
are unchanged.

### Raw artifacts and reproduction

Absolute artifact root:
`/nfs/home/zhangziqing/hier-model/XiangShan/build/loadqueuerar-cosim-20261123/`

- `summary.json`, `measurements.json`, `measurements/`: primary statistics,
  individual outputs, process resource logs and exact run commands.
- `environment.json`, `resources.json`, `source-hashes.json`,
  `model-boundary.json`: tools, configuration, build costs and model inventory.
- `trace.py`, `trace.bin`, `trace.bin.golden`, `trace.json`,
  `output-coverage.json`, `extra-checks.json`, `extra-trace.bin.golden`:
  stimulus, production-RTL oracle, coverage, hashes and additional checks.
- `src/`: local Scala sources, original RAR snapshot and common C++ harness.
  `rtl/`, `storage-rtl/`, `cosim/`: generated models, descriptors, binaries,
  build commands/logs and cycle-comparison logs.
- `run_scala.py`, `generate.py`, `build.py`, `measure.py`, `summarize.py`,
  `extra_check.py`: generation/build/measurement/check drivers. Scala compiler,
  plugin and project classpaths are recorded alongside them for this workspace.

From the XiangShan root, `run_scala.py compile` compiles the absolute `src/*.scala`
paths; `generate.py` emits the three variants; `build.py` builds them; `trace.py`
creates the input trace. Run the RTL executable in `capture` mode to create the
`.golden` trace, then each executable in `check` mode before `measure.py`.
`extra_check.py` creates and verifies the additional reset/redirect trace.
Benchmark sources and generated artifacts remain ignored and are not committed
as Scala tests. Production code and slides are unchanged.
