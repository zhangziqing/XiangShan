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

The dependency stack starts from accepted base `ec165efa7a` and preserves its
consolidated Storage files and generation-wide RTL/CoSim selection. Prerequisite
PRs provide typed function IR, C++ emission, callback execution, partial resets,
and the separately reviewed Storage contracts and backend lowering. The RAR PR
changes the module's use of those capabilities and records its validation here.

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

The prerequisite backend mechanisms used by this migration are:

- Per-session `FunctionId` values span captured Chisel modules, saved CHIRRTL and
  lowered FIRRTL, JSON, generated SV bridges and named `intention_function_<id>`
  C++ functions. Concrete Data classes and their methods survive invocation;
  the final descriptor contains compiler IR and metadata rather than closures.
- Pure, combinational callbacks support `update`, `foreach` and the CAM APIs.
  Hardware context is passed as explicit typed parameters. External hardware
  references, stateful bodies and unsupported primitives fail generation.
  `camRead` overflow checks use the caller's Chisel clock, reset and when scope.
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
| `mill -i -j 4 xiangshan.compile` | PASS; final log `mill-compile-camread.log` |
| Final Storage RTL vs original RTL, two traces | PASS; 65,532 checked cycles per trace, before and after each edge |
| Final CoSim vs original RTL, same two traces | PASS; same cycle checks, including invalid response hold |
| UInt/SInt 1/8/63/64-bit and Bundle/Vec callback matrix | PASS; 9 models x 5,000 cycles in each indexed-read bridge mode |
| All callback APIs and mixed write ordering | PASS; 50,000 cycles against independent RegStorage implementation, repeated after the clocked camRead assertion fix |
| camRead overflow assertion timing | PASS in both backends; reset and guard suppression, combinational transients, old-state edge sampling, and expected rising-edge failure |
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
- `camread-model/check.log`, `camread-rtl/check.log`,
  `camread-overflow-checks.json`: assertion timing regression and expected failures.
- `rar-clocked-comparison.json`: after the assertion fix, all RAR C++/headers
  remain byte-identical to the measured model; SV differs only in source-location
  comments. `correctness-after-camread.json` records both repeated trace checks.
- `mill-compile-camread.log`: successful full project compile after the final
  assertion fix; the final generated Mill cache used temporary local disk to
  avoid the account space limit. Duplicated Mill
  cache and obsolete intermediate object directories were removed after an
  account space limit interrupted setup; final binaries and raw logs remain.

The immutable original traces, RTL source snapshot and baseline binaries remain
under `/nfs/home/zhangziqing/hier-model/XiangShan/build/loadqueuerar-cosim-20261123/`.
Use `rtl-final/obj/VRarBench` or `packed-final/obj/VRarBench` with a golden trace
and `check` to repeat cycle checks; use `measurement-final/VRarBench` with the
saved scripts for timed replay. Rebuilding Scala requires the normal project
compile and initialized dependencies; cached Scala classpaths are local build
conveniences. No slides were changed.
