# Storage intention model

`ModelStorage` is an explicitly selected software backend for the existing
[`Storage[T]`](../utils/QueueModel/Storage.scala) contract. The default hardware
backend remains `RegStorage`. The first executable slice implements `read`,
`write`, and `writeField`; CAM, update, and foreach have reserved IR types but
reject executable use until callback lowering is implemented.

## Use and build

Following PR #21, `Storage` is an ordinary Scala object with public operations,
instantiated directly in its owning module. Mix `StorageModelOwner` into modules
that select `ModelStorage`; it finishes capture at module-body end using Chisel's
public lifecycle API. Provide one `StorageIRSession` for the whole elaboration.
`StorageModelStage.emit` creates that session and returns the sealed descriptors:

```scala
import chisel3._
import IntentionModel._
import java.nio.file.Paths

class ExampleStorage(implicit session: StorageIRSession) extends Module with StorageModelOwner {
  val storage = new ModelStorage(UInt(16.W), 4, Some(Seq.fill(4)(0.U(16.W))))
  val io = IO(new Bundle {
    val index = Input(UInt(3.W))
    val enable = Input(Bool())
    val data = Input(UInt(16.W))
    val result = Output(UInt(16.W))
  })
  io.result := storage.read(io.index)
  when(io.enable) { storage.write(io.index, io.data) }
}

object EmitExample extends App {
  val models = StorageModelStage.emit(
    s => { implicit val session: StorageIRSession = s; new ExampleStorage },
    Paths.get("build"))
}
```

Select read bridge granularity per storage:

```scala
val storage = new ModelStorage(gen, entries, initial,
  readBridge = StorageReadBridgeMode.PerStorage)
```

`PerRead` is the default: each source read has a separate bridge and query.
`PerStorage` creates one shared read bridge and one instance-specific DPI query
for all read sites. Any address change evaluates every port in that group; this
trades query boundaries for potentially redundant indexed loads. Both modes
preserve combinational reads, operation/result IDs and the same cycle bridge.
The choice is an elaboration setting, independent of storage capacity and state
semantics. The descriptor emitter provides both query interfaces.

Run from the XiangShan root, or pass the runtime header path explicitly as the
third argument to `emit`. The resulting files are:

| Path under the build directory | Contents |
| --- | --- |
| `rtl/` | Split SystemVerilog, including inline DPI BlackBoxes |
| `.storage-stage/` | Chisel Stage output, separate from split RTL |
| `generated-src/storage-<id>.json` | Validated schema 1 descriptor per instance |
| `generated-src/storage_dpi.cpp` | Descriptor-driven DPI entry points |
| `generated-src/storage.hpp` | Copy of the C++ runtime header |

The existing [`difftest/Makefile`](../../../../difftest/Makefile) discovers
`build/generated-src/*.cpp` and adds that directory to the include path. An
external simulator build must compile `storage_dpi.cpp`, make `storage.hpp` and
the simulator's `svdpi.h` visible, and enable SystemVerilog DPI. The runtime
requires C++14 or later; local verification used C++17.
Add the emitted `rtl/` files to that simulator's RTL source list; this helper does
not replace the normal full-CPU generation flow.

The descriptor CLI works without elaborating a design:

```sh
mill -i xiangshan.runMain IntentionModel.StorageDescriptorMain --check build/generated-src
mill -i xiangshan.runMain IntentionModel.StorageDescriptorMain --emit build/generated-src
```

`--check` validates every descriptor, checks filename/ID correspondence, and
compares the C++ file with its exact regenerated contents. `--emit` regenerates
the C++ from saved descriptors. Neither command regenerates RTL or copies the
runtime header. After a bridge ABI change, re-elaborate RTL and C++ together;
descriptor regeneration alone does not update inline SV. C++ emission sorts instances by Storage ID; emitting a new model
set also removes obsolete, valid `storage-<id>.json` descriptors from its output
directory. Treat that directory as generated output.

For custom Stage integration, use `top.XiangShanStage` and emit `session.models`
through `StorageDPIEmitter` after elaboration. A plain Chisel Stage does not run
the required `StorageCapturePhase`.

## Representation and traceability

The source module owns directly instantiated `Storage` objects. A Storage is not
a Chisel module and does not add a module boundary.
The abstraction hierarchy consists of one storage instance, its entry layout,
ordered operations, and operand/result identities. The executable hierarchy has
one cycle bridge per storage, plus either one read bridge per source read call
or one shared read bridge per storage. Entry state resides in the C++ model.
Bridge count is independent of entry capacity.

[`StorageIR.scala`](StorageIR.scala) defines the immutable serialization boundary.
It contains instance ID/path, capacity, packed type layout, clock/reset targets,
initial state, values, an expression DAG, and one ordered operation vector.
Each operation has a source location, ID, and ordinal. Full and field writes
remain interleaved in Scala call order. Numeric operation IDs identify call
sites; vector order determines priority.

`SignalRef` targets refer to elaborated FIRRTL before optimization and renaming.
They preserve source-instance traceability; they are not a contract for final
Verilog net names. Operation locations use JVM call-site file/line metadata;
columns are optional. No Scala callback, Chisel `Data`, or `Module` is retained
in the sealed IR.

[`StorageIRBuilder.scala`](StorageIRBuilder.scala) owns temporary `Data` captures
per instance. `StorageModelOwner.atModuleBodyEnd` closes capture and creates the
cycle bridge and any shared read bridge.
Stage annotation conversion then resolves absolute targets, validates and seals
the IR, records it in the session, and releases captured `Data` references.
Further capture is rejected; sealing again returns the same immutable IR.
The session owns ID allocation, so a fresh elaboration starts a fresh ID space.
This lifecycle does not depend on garbage collection.

At each API call, packed wires capture operands in their original Chisel
conditional scope, including `when.cond` for writes. Each read result carries
`StorageReadResultAnnotation`; it connects directly to its read BlackBox or uses
an anchored packed wire connected to the shared bridge at module-body end.
[`StorageCapturePhase.scala`](StorageCapturePhase.scala) lifts only explicitly
annotated packed-wire and read-instance declarations to module scope, supplies
zero defaults, preserves guarded assignments, and adds anchored bridge
connections. This avoids escaping Chisel conditional scopes. It does not infer
storage patterns from signal names or modify firtool.

## Functional and cycle contract

The observable contract is the value of each read and the committed packed state
at each active clock edge. The model has no request/response handshake or added
read latency. Combinational operations around these APIs remain in generated
RTL; the C++ model implements storage state and ordered updates.

- A read is a pure query of committed state. It has no forwarding from writes
  recorded earlier in the same Scala body. Dynamic out-of-range reads return
  zero; dynamic out-of-range writes have no effect. A static out-of-range read
  fails at elaboration.
- Generated cycle functions apply writes directly in IR order. Capacity, entry
  width and field slices are compile-time constants. The bridge preserves the
  original address widths and packs reset, guards, addresses and payloads into
  64-bit DPI words. A field never crosses a word; padding is zero. For the
  72-entry FreeList, all four writes and reset occupy one 61-bit word.
- Each storage has one rising-edge cycle boundary and two physical state
  versions. At posedge, one C++ call computes the unpublished version from the
  current version and applies ordered writes. Reset installs constants directly
  in the unpublished version. The SV bridge then changes a one-bit epoch with a
  nonblocking assignment. Read functions use that epoch to select a version, so
  they remain pure queries: callbacks cannot expose new state to downstream
  registers or other storages before those consumers sample the old epoch.
  Epoch also refreshes reads whose index remains unchanged. Address changes
  still evaluate reads between edges, without extra read latency.
- An edge with no enabled writes and no applicable initialized reset skips the
  C++ call and epoch change. Unspecified initialization does not add a reset
  enable. There is no SV operand snapshot, separate pending event, per-edge
  allocation or runtime descriptor traversal. Version publication replaces the
  earlier operand-sampling bridge. The normal update copies one native entry
  array before writing; state memory is twice the single-version array size.
  This cost scales with capacity and must be remeasured for large storages.
- Enabled writes execute in operation-vector order. Later writes win for every
  overlapping bit, including overlaps between whole-entry, aggregate-field,
  and leaf-field writes. Unwritten bits retain their prior value.
- Reset is synchronous and active high. With constant initialization, reset
  replaces every entry and wins over writes on that edge. With unspecified
  initialization, reset does not inhibit writes, matching an ordinary Chisel
  `Reg`. Initial constants take effect on reset, not at model construction.
- Execution is two-state. Each state version stores one packed integer per entry;
  reads return that integer directly, without known masks or per-bit output
  reconstruction. Each entry uses the smallest native unsigned integer type that
  contains its packed width, in a fixed `std::array`. Uninitialized bits use zero
  as the executable's deterministic choice, including before the first reset. The IR still records unspecified
  initialization separately from reset constants; behavior before a bit is
  initialized or written is outside the hardware equivalence claim. Bridge
  inputs assume defined reset, guard, address and enabled payloads. Input/output
  `x`/`z` propagation and four-state simulation are outside the current contract.
- Values use explicit packed widths. Chisel assignment to a prototype normalizes
  each write payload before packing: unsigned widening zero-extends, signed
  widening sign-extends, and narrowing discards high bits. Runtime masks constrain
  each field and avoid a shift by 64. Initialization literals are normalized to
  the declared entry width, including signed extension.

[`StorageLayout.scala`](StorageLayout.scala) defines
`chisel-asUInt-lsb-first-v1`: Bundle fields follow Chisel `getElements` order,
starting at the least significant bit; Vec element 0 occupies the least
significant element slice. Nested leaves have static offsets, widths and
signedness. `FieldOf.path` resolves to these slices during generation; offset
and width are not runtime inputs. Whole-entry `asTypeOf` reconstructs the same
layout at a read result.

The C++ [`storage.hpp`](../../../test/scala/csrc/storage/storage.hpp) implements
the state semantics. Generated C++ declares one file-scope static pair of
`StorageModel` versions per Storage ID, initialized by normal C++ startup rather
than a DPI call. Each version specializes capacity and packed width and owns its
entry array; reset constants are emitted separately. Initialization has no dependency on
another translation unit or simulator API. Earlier constructor instrumentation
verified startup before `main`; the fixed-array representation additionally
requires no heap allocation or function-local initialization guard.
Instance-specific read functions access their model directly, without a runtime
Storage ID switch; source operation IDs remain in the IR and annotations.
The models live for the simulator process. There are no DPI create/destroy calls or SV
`initial`/`final` lifecycle hooks. The elaborated IDs identify one static design
hierarchy, so a simulator process must instantiate that generated top once;
instantiating the same generated top multiple times would alias equal IDs. A new
simulator process gets fresh C++ state. Within a process, constant initialization
still takes effect on reset, as described above. Calls assume serial simulator
execution; concurrent DPI calls are not supported. Clocked inputs must be stable
for the simulator's edge sampling, and combinational paths must settle before
their next sampling edge.

Descriptor structure, widths and operation references are checked once in Scala
before C++ emission. The generated runtime trusts that validated IR and
its matching DPI signatures; it does not repeat constructor validation, scan read
operation IDs, check write counts, or wrap every call in an exception handler.
Address bounds and width masks remain part of the functional model contract.

## Supported boundary

| Capability | Executable support |
| --- | --- |
| UInt, SInt, ordinary Bundle and homogeneous Vec | Fixed positive widths; total entry width at most 64 bits |
| Address | UInt, 1 through 64 bits; captured without narrowing |
| Initialization | Every entry supplied as a complete constant, or explicitly unspecified |
| Read, full write, static field write | Supported, including nested conditional calls |
| Clock and reset | One implicit clock/reset per storage; synchronous active-high reset |
| CAM, update, foreach callbacks | Stable IR vocabulary only; execution rejected |
| Custom/opaque Records, zero/unknown widths, wider entries or addresses | Rejected |
| Dynamic initialization, AsyncReset, per-operation clock/reset override | Rejected |

The reserved expression DAG stores field/parameter references, literals,
typed unary/binary operators, and muxes through `ExprId`. It never serializes a
closure. Validation checks references, topological order, types, constants,
layout, operation results and ordinals. `requireDPI` adds executable capability
checks; successful serialization alone does not imply executable support.

The intended state correspondence is bit-for-bit equality with `RegStorage` for
initialized/written bits, with identical read observations and edge update
order. Composition relies on compatible clock/reset contracts and the stated
sampling discipline at connected modules. This contract does not constitute a formal equivalence or full-CPU proof.

## Source and generated files

Maintain the Scala sources in this directory and the C++ runtime header at
`src/test/scala/csrc/storage/storage.hpp`. That header is runtime implementation
code even though its existing location is under the test tree.

`StorageDescriptorMain` is the descriptor checking/regeneration CLI;
`StorageModelStage` is an integration helper. `StorageReadBatchBridge` implements
the optional shared-read configuration. These are maintained sources, as are the
inline SV templates in the bridge classes.

Descriptors, emitted DPI C++, the copied runtime header, inline bridge SV,
CIRCT RTL and Verilator C++ belong in the build directory and are reproducible
outputs. Local verification harnesses, performance drivers and measurement
reports are outside this implementation change.

Compile the Scala implementation with `mill -i xiangshan.compile`. Validate
saved descriptor/C++ pairs with the CLI shown above. An external simulator build
must also compile the generated C++ and link it with the generated RTL.
