// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3.Data._
import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.HasBlackBoxInline
import circt.stage.FirtoolOption
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import utils.QueueModel.{FieldOf, IntentionCallback, Storage}

/** Software implementation selected by the generation scope for Storage calls.
  * Direct construction remains available for explicit backend verification.
  */
class ModelStorage[T <: Data](
  gen: T, entries: Int, initContents: Option[Seq[T]] = None,
  readBridge: StorageReadBridgeMode = StorageReadBridgeMode.PerRead
)(
  implicit session: StorageIRSession
) extends Storage[T](entries) {
  private val owner = Module.currentModule match {
    case Some(module: IntentionCallback) => module
    case _ => throw new IllegalArgumentException("ModelStorage requires an IntentionCallback module")
  }
  private val clock = Module.clock
  private val reset = Module.reset
  require(!reset.isInstanceOf[AsyncReset], "ModelStorage requires a synchronous Bool reset")
  require(entries > 0, "Storage capacity must be positive")
  private val layout = StorageLayout.fromData(gen)
  require(layout.totalWidth <= 64, "ModelStorage supports entries of at most 64 bits")
  private val initial: InitialState = initContents match {
    case None => UnspecifiedInitialState
    case Some(values) =>
      require(values.size == entries, "initial contents must describe every entry")
      ConstantInitialState(values.map { value =>
        val bits = value.litOption.getOrElse(
          throw new IllegalArgumentException("ModelStorage initialization requires complete constants"))
        bits & StorageLayout.mask(layout.totalWidth)
      }.toVector)
  }
  private val builder = new StorageIRBuilder(session.allocate(), entries, layout, initial)
  private val wires = ArrayBuffer.empty[UInt]
  private val reads = ArrayBuffer.empty[StorageReadBridge]
  private val batchReads = ArrayBuffer.empty[(ValueId, UInt)]
  private val connections = ArrayBuffer.empty[(Data, Data)]

  private def source: SourceLoc = {
    val frame = Thread.currentThread().getStackTrace.find { f =>
      val name = f.getClassName
      f.getLineNumber > 0 && f.getFileName != null && f.getFileName != "ModelStorage.scala" &&
        !Seq("java.", "scala.", "chisel3.").exists(name.startsWith)
    }.getOrElse(throw new IllegalArgumentException("Storage operation has no source location"))
    SourceLoc(frame.getFileName, frame.getLineNumber)
  }
  private def context(): Unit = {
    require(Module.currentModule.contains(owner), "ModelStorage operations must remain in their owning module")
    require(Module.clock eq clock, "ModelStorage does not support per-operation clock overrides")
    require(Module.reset eq reset, "ModelStorage does not support per-operation reset overrides")
  }
  private def capture(data: UInt, width: Int, tpe: StorageType): ValueId = {
    val wire = Wire(UInt(width.W))
    wire := data
    wires += wire
    builder.capture(wire, tpe)
  }
  private def address(index: UInt): ValueId = {
    require(index.getWidth > 0 && index.getWidth <= 64, "ModelStorage index width must be in 1..64")
    capture(index, index.getWidth, UIntType(index.getWidth))
  }
  private def query(index: UInt, location: SourceLoc): T = {
    context()
    val capturedIndex = address(index)
    val value = readBridge match {
      case StorageReadBridgeMode.PerRead =>
        val bridge = Module(new StorageReadBridge(layout.totalWidth, builder.id, builder.nextOp))
        bridge.io.index := index
        reads += bridge
        bridge.io.value
      case StorageReadBridgeMode.PerStorage =>
        val result = Wire(UInt(layout.totalWidth.W))
        wires += result
        batchReads += capturedIndex -> result
        result
    }
    val (op, result) = builder.read(capturedIndex, value, location)
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = StorageReadResultAnnotation(value.toAbsoluteTarget,
        builder.id.value, op.value, result.value)
    })
    value.asTypeOf(gen).readOnly
  }
  override final def read(index: Int): T = {
    require(index >= 0 && index < entries, "static storage index is out of range")
    query(index.U, source)
  }
  override final def read(index: UInt): T = query(index, source)

  private def recordWrite[D <: Data](
    index: UInt, prototype: D, content: D, path: Option[Vector[FieldStep]], location: SourceLoc
  ): Unit = {
    context()
    val guard = capture(when.cond.asUInt, 1, UIntType(1))
    val capturedIndex = address(index)
    val normalized = Wire(prototype.cloneType)
    normalized := content
    val tpe = path.map(StorageLayout.field(layout, _)._1).getOrElse(layout.shape)
    val value = capture(normalized.asUInt, StorageLayout.width(tpe), tpe)
    builder.write(guard, capturedIndex, value, path, location)
  }
  override final def write(index: UInt, content: T): Unit =
    recordWrite(index, gen, content, None, source)
  override final def writeField[D <: Data](index: UInt, field: FieldOf[T, D], content: D): Unit =
    recordWrite(index, field.select(gen.cloneType), content, Some(StorageLayout.path(field.path)), source)

  private def unsupported[A](operation: String): A =
    throw new IllegalArgumentException(s"ModelStorage $operation callback lowering is not implemented")
  override final def camHitVec[A](f: (T, A) => Bool, arg: A): Seq[Bool] = unsupported("camHitVec")
  override final def camHitIdx[A](f: (T, A) => Bool, arg: A): Seq[UInt] = unsupported("camHitIdx")
  override final def camRead[A](f: (T, A) => Bool, arg: A, maxHits: Int): Seq[(T, Bool)] = unsupported("camRead")
  override final def camWrite[P](f: (T, P) => Bool, param: P, content: T): Unit = unsupported("camWrite")
  override final def camWrite[P, D <: Data](
    f: (T, P) => Bool, param: P, field: FieldOf[T, D], content: D
  ): Unit = unsupported("camWriteField")
  override final def update(f: T => T): Unit = unsupported("update")
  override final def foreach[A <: Data](f: T => A): Seq[A] = unsupported("foreach")

  owner.registerIntentionCallback(() => {
    val ports = builder.writes
    val widths = ports.map(port => builder.data(port.index).getWidth -> builder.data(port.value).getWidth)
    val cycle = Module(new StorageCycleBridge(builder.id, widths, initial.isInstanceOf[ConstantInitialState]))
    cycle.io.clock := clock
    // A direct Bool connection constrains an inferred Reset to synchronous.
    // Casting with asBool would silently accept an asynchronous parent reset.
    cycle.io.reset := reset
    ports.zipWithIndex.foreach { case (port, i) =>
      connections ++= Seq(cycle.io.guard(i) -> builder.data(port.guard),
        cycle.io.index(i) -> builder.data(port.index), cycle.io.value(i) -> builder.data(port.value))
    }
    reads.foreach(r => connections += r.io.epoch -> cycle.io.epoch)
    if (batchReads.nonEmpty) {
      val batch = Module(new StorageReadBatchBridge(layout.totalWidth, builder.id, batchReads.size))
      connections += batch.io.epoch -> cycle.io.epoch
      batchReads.zipWithIndex.foreach { case ((index, result), i) =>
        connections ++= Seq(batch.io.index(i) -> builder.data(index), result -> batch.io.value(i))
      }
    }
    builder.closeCapture()
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = StorageWiringAnnotation(owner.name,
        wires.map(_.toTarget.ref).toVector, reads.map(_.instanceName).toVector,
        reads.map(_.io.index.toRelativeTarget(Some(owner))).toVector,
        connections.map { case (sink, src) =>
          sink.toRelativeTarget(Some(owner)) -> src.toRelativeTarget(Some(owner))
        }.toVector)
    })
    annotate(new ChiselAnnotation {
      def toFirrtl: Annotation = {
        val ir = builder.seal(owner.toAbsoluteTarget.serialize,
          SignalRef(clock.toAbsoluteTarget.serialize, UIntType(1)),
          ResetSpec(SignalRef(reset.toAbsoluteTarget.serialize, UIntType(1)), true, true),
          data => SignalRef(data.toAbsoluteTarget.serialize, UIntType(data.getWidth)))
        session.record(ir)
        StorageIRAnnotation(StorageIRCodec.encode(ir))
      }
    })
  })
}

/** Compatibility alias for explicitly constructed synchronous software models.
  * Backend-independent modules use IntentionCallback and the Storage declaration.
  */
trait StorageModelOwner extends RequireSyncReset with IntentionCallback

sealed trait StorageReadBridgeMode
object StorageReadBridgeMode {
  case object PerRead extends StorageReadBridgeMode
  case object PerStorage extends StorageReadBridgeMode
}

/** One pure current-state query per source read, independent of entry count. */
class StorageReadBridge(width: Int, storageId: StorageId, opId: OpId)
  extends BlackBox with HasBlackBoxInline {
  require(width > 0 && width <= 64, "DPI read width must be in 1..64")
  override def desiredName: String = s"StorageReadBridge_${storageId.value}_${opId.value}"
  val io = IO(new Bundle {
    val epoch = Input(Bool())
    val index = Input(UInt(64.W))
    val value = Output(UInt(width.W))
  })
  setInline(s"$desiredName.sv",
    s"""module $desiredName(
       |  input epoch, input [63:0] index,
       |  output logic [${width - 1}:0] value
       |);
       |  import "DPI-C" function longint unsigned ${StorageDPIEmitter.readName(storageId)}(
       |    input longint unsigned index, input bit epoch);
       |  always_comb begin
       |    value = $width'(${StorageDPIEmitter.readName(storageId)}(index, epoch));
       |  end
       |endmodule
       |""".stripMargin)
}

private[IntentionModel] class StorageReadBatchIO(width: Int, count: Int) extends Record {
  val elements = ListMap((Seq("epoch" -> Input(Bool())) ++ (0 until count).flatMap(i => Seq(
    s"index_$i" -> Input(UInt(64.W)), s"value_$i" -> Output(UInt(width.W))
  ))): _*)
  def epoch: Bool = elements("epoch").asInstanceOf[Bool]
  def index(i: Int): UInt = elements(s"index_$i").asInstanceOf[UInt]
  def value(i: Int): UInt = elements(s"value_$i").asInstanceOf[UInt]
}

/** All read sites share one query boundary. Any address change refreshes every port. */
private[IntentionModel] class StorageReadBatchBridge(width: Int, storageId: StorageId, count: Int)
  extends BlackBox with HasBlackBoxInline {
  require(width > 0 && width <= 64, "DPI read width must be in 1..64")
  require(count > 0, "A batch read bridge requires at least one read")
  override def desiredName: String = s"StorageReadBatchBridge_${storageId.value}"
  val io = IO(new StorageReadBatchIO(width, count))
  private val ports = (0 until count).map(i =>
    s", input [63:0] index_$i, output logic [${width - 1}:0] value_$i").mkString
  private val args = (0 until count).map(i =>
    s", input longint unsigned index_$i, output longint unsigned bits_$i").mkString
  private val words = (0 until count).map(i => s"longint unsigned bits_$i;").mkString("\n")
  private val inputs = (0 until count).map(i => s", index_$i, bits_$i").mkString
  private val outputs = (0 until count).map(i => s"value_$i = bits_$i[${width - 1}:0];").mkString("\n")
  setInline(s"$desiredName.sv",
    s"""module $desiredName(input epoch$ports);
       |  import "DPI-C" function void ${StorageDPIEmitter.readBatchName(storageId)}(input bit epoch$args);
       |  $words
       |  always_comb begin
       |    ${StorageDPIEmitter.readBatchName(storageId)}(epoch$inputs);
       |    $outputs
       |  end
       |endmodule
       |""".stripMargin)
}

final case class StorageCycleField(name: String, width: Int, offset: Int) {
  def end: Int = offset + width
  def word: Int = offset / 64
  def shift: Int = offset % 64
}

/** Fixed operand layout shared by the SV sampler and generated C++ decoder.
  * Fields never cross a 64-bit DPI word; padding is always zero.
  */
final case class StorageCyclePacking(writeWidths: Vector[(Int, Int)]) {
  val fields: Vector[StorageCycleField] = {
    val widths = Vector("reset" -> 1) ++ writeWidths.zipWithIndex.flatMap { case ((index, value), i) =>
      Vector(s"guard_$i" -> 1, s"index_$i" -> index, s"value_$i" -> value)
    }
    var next = 0
    widths.map { case (name, width) =>
      require(width > 0 && width <= 64, "cycle operand width must be in 1..64")
      if (next % 64 + width > 64) next += 64 - next % 64
      val field = StorageCycleField(name, width, next)
      next = field.end
      field
    }
  }
  val totalWidth: Int = fields.last.end
  val words: Int = (totalWidth + 63) / 64
  def field(name: String): StorageCycleField = fields.find(_.name == name).get
}

object StorageCyclePacking {
  def fromIR(ir: StorageIR): StorageCyclePacking = {
    val types = ir.values.map(v => v.id -> v.tpe).toMap
    def widths(index: Operand, value: Operand): (Int, Int) =
      StorageLayout.width(types(index.value)) -> StorageLayout.width(types(value.value))
    StorageCyclePacking(ir.operations.collect {
      case WriteOp(_, _, _, index, value, _) => widths(index, value)
      case WriteFieldOp(_, _, _, index, _, value, _) => widths(index, value)
    })
  }
}

class StorageCycleIO(widths: Vector[(Int, Int)]) extends Record {
  val elements = ListMap((Seq(
    "clock" -> Input(Clock()), "reset" -> Input(Bool()),
    "epoch" -> Output(Bool())
  ) ++ widths.zipWithIndex.flatMap { case ((index, value), i) => Seq(
    s"guard_$i" -> Input(Bool()), s"index_$i" -> Input(UInt(index.W)), s"value_$i" -> Input(UInt(value.W))
  ) }): _*)
  def clock: Clock = elements("clock").asInstanceOf[Clock]
  def reset: Bool = elements("reset").asInstanceOf[Bool]
  def epoch: Bool = elements("epoch").asInstanceOf[Bool]
  def guard(i: Int): Bool = elements(s"guard_$i").asInstanceOf[Bool]
  def index(i: Int): UInt = elements(s"index_$i").asInstanceOf[UInt]
  def value(i: Int): UInt = elements(s"value_$i").asInstanceOf[UInt]
}

/** Computes an unpublished state version at the edge; NBA switches read visibility.
  * Reads using the old epoch remain stable until downstream registers sample.
  */
class StorageCycleBridge(storageId: StorageId, widths: Vector[(Int, Int)], initialized: Boolean)
  extends BlackBox with HasBlackBoxInline {
  private val packing = StorageCyclePacking(widths)
  override def desiredName: String = s"StorageCycleBridge_${storageId.value}"
  val io = IO(new StorageCycleIO(widths))
  private val args = (0 until packing.words).map(i => s"input longint unsigned inputs_$i").mkString(", ")
  private val active = {
    val events = (if (initialized) Seq("reset") else Seq.empty) ++ widths.indices.map(i => s"guard_$i")
    if (events.isEmpty) "1'b0" else events.mkString(" || ")
  }
  private val packed = {
    var previous = 0
    packing.fields.flatMap { field =>
      val padding = if (field.offset > previous) Seq(s"${field.offset - previous}'d0") else Seq.empty
      previous = field.end
      padding :+ field.name
    }.reverse.mkString("{", ", ", "}")
  }
  private val inputs = (0 until packing.words).map { i =>
    val high = math.min((i + 1) * 64, packing.totalWidth) - 1
    s"64'(cycle_inputs[$high:${i * 64}])"
  }.mkString(", ")
  private val svPorts = widths.zipWithIndex.map { case ((index, value), i) =>
    s", input guard_$i, input [${index - 1}:0] index_$i, input [${value - 1}:0] value_$i"
  }.mkString
  setInline(s"$desiredName.sv",
    s"""module $desiredName(
       |  input clock, input reset,
       |  output bit epoch$svPorts
       |);
       |  import "DPI-C" function void ${StorageDPIEmitter.cycleName(storageId)}(input bit epoch, $args);
       |  wire [${packing.totalWidth - 1}:0] cycle_inputs = $packed;
       |  initial begin
       |    epoch = 0;
       |  end
       |  always @(posedge clock) begin
       |    if ($active) begin
       |      ${StorageDPIEmitter.cycleName(storageId)}(epoch, $inputs);
       |      epoch <= ~epoch;
       |    end
       |  end
       |endmodule
       |""".stripMargin)
}

/** C++ consumes only sealed descriptors; SV/C signatures share this emitter. */
object StorageDPIEmitter {
  def readName(id: StorageId): String = s"intention_storage_read_${id.value}"
  def readBatchName(id: StorageId): String = s"intention_storage_read_batch_${id.value}"
  def cycleName(id: StorageId): String = s"intention_storage_cycle_${id.value}"
  def writeSpecs(ir: StorageIR): Vector[(Int, Int, Int)] = ir.operations.collect {
    case WriteOp(id, _, _, _, _, _) => (id.value, 0, ir.entry.totalWidth)
    case WriteFieldOp(id, _, _, _, path, _, _) =>
      val (_, offset, width) = StorageLayout.field(ir.entry, path)
      (id.value, offset, width)
  }

  def cpp(models: Vector[StorageIR]): String = {
    models.foreach(StorageIRValidation.requireDPI)
    require(models.map(_.id).distinct.size == models.size, "duplicate storage ID in executable")
    val ordered = models.sortBy(_.id.value)
    def u(bits: BigInt): String = s"UINT64_C(0x${bits.toString(16)})"
    val storageModels = ordered.map { ir =>
      val initial = ir.initial match {
        case ConstantInitialState(bits) =>
          s"static const Model_${ir.id.value}::State initial_${ir.id.value}{{${bits.map(u).mkString(", ")}}};"
        case UnspecifiedInitialState => ""
      }
      s"""using Model_${ir.id.value} = intention::StorageModel<${ir.entries}, ${ir.entry.totalWidth}>;
         |static std::array<Model_${ir.id.value}, 2> storage_${ir.id.value};
         |$initial""".stripMargin
    }.mkString("\n")
    val readFunctions = ordered.filter(_.operations.exists(_.isInstanceOf[ReadOp])).map { ir =>
      s"""extern "C" unsigned long long ${readName(ir.id)}(unsigned long long index, svBit epoch) {
         |  return storage_${ir.id.value}[epoch != 0].read(index);
         |}""".stripMargin
    }.mkString("\n")
    val readBatches = ordered.flatMap { ir =>
      val reads = ir.operations.collect { case r: ReadOp => r }
      if (reads.isEmpty) None else {
        val args = reads.indices.map(i =>
          s", unsigned long long index_$i, unsigned long long *value_$i").mkString
        val outputs = reads.indices.map(i => s"*value_$i = storage_${ir.id.value}[epoch != 0].read(index_$i);").mkString("\n")
        Some(s"""extern "C" void ${readBatchName(ir.id)}(svBit epoch$args) {
                |  $outputs
                |}""".stripMargin)
      }
    }.mkString("\n")
    val cycleFunctions = ordered.map { ir =>
      val specs = writeSpecs(ir)
      val packing = StorageCyclePacking.fromIR(ir)
      val args = (0 until packing.words).map(i => s"unsigned long long inputs_$i").mkString(", ")
      def operand(name: String): String = {
        val field = packing.field(name)
        val word = s"inputs_${field.word}"
        if (field.width == 64) word
        else {
          val shifted = if (field.shift == 0) word else s"($word >> ${field.shift})"
          s"($shifted & ${u(StorageLayout.mask(field.width))})"
        }
      }
      val reset = ir.initial match {
        case _: ConstantInitialState =>
          s"if (${operand("reset")}) { next.reset(initial_${ir.id.value}); return; }"
        case UnspecifiedInitialState => "(void)inputs_0;"
      }
      val writes = specs.zipWithIndex.map { case ((_, offset, width), i) =>
        s"next.write<$offset, $width>(${operand(s"guard_$i")} != 0, ${operand(s"index_$i")}, ${operand(s"value_$i")});"
      }.mkString("\n")
      s"""extern "C" void ${cycleName(ir.id)}(svBit epoch, $args) {
         |  auto &next = storage_${ir.id.value}[epoch == 0];
         |  $reset
         |  next = storage_${ir.id.value}[epoch != 0];
         |  $writes
         |}""".stripMargin
    }.mkString("\n")
    s"""// Generated from storage IR schema 1. Do not edit.
       |#include "storage.hpp"
       |#include "svdpi.h"
       |$storageModels
       |$readFunctions
       |$readBatches
       |$cycleFunctions
       |""".stripMargin
  }

  def emit(models: Vector[StorageIR], directory: Path): Unit = {
    val source = cpp(models)
    Files.createDirectories(directory)
    val stale = Files.newDirectoryStream(directory, "storage-*.json")
    try {
      val keep = models.map(ir => s"storage-${ir.id.value}.json").toSet
      val iterator = stale.iterator()
      while (iterator.hasNext) {
        val file = iterator.next()
        if (!keep(file.getFileName.toString)) {
          val old = StorageIRCodec.decode(new String(Files.readAllBytes(file), UTF_8))
          require(file.getFileName.toString == s"storage-${old.id.value}.json", "unexpected descriptor filename")
          Files.delete(file)
        }
      }
    } finally stale.close()
    models.foreach(ir => Files.write(directory.resolve(s"storage-${ir.id.value}.json"),
      StorageIRCodec.encode(ir).getBytes(UTF_8)))
    Files.write(directory.resolve("storage_dpi.cpp"), source.getBytes(UTF_8))
  }
}

/** Validate generated artifacts or regenerate C++ from saved JSON descriptors. */
object StorageDescriptorMain {
  def main(args: Array[String]): Unit = {
    require(args.length == 2 && Set("--check", "--emit")(args(0)),
      "Usage: StorageDescriptorMain (--check|--emit) <generated-src-directory>")
    val directory = Paths.get(args(1))
    val stream = Files.newDirectoryStream(directory, "storage-*.json")
    val files = try stream.iterator().asScala.toVector.sortBy(_.getFileName.toString) finally stream.close()
    val decoded = files.map(file => file -> StorageIRCodec.decode(new String(Files.readAllBytes(file), UTF_8)))
    decoded.foreach { case (file, model) =>
      require(file.getFileName.toString == s"storage-${model.id.value}.json", "descriptor filename/ID mismatch")
    }
    val models = decoded.map(_._2).sortBy(_.id.value)
    val expected = StorageDPIEmitter.cpp(models)
    val source = directory.resolve("storage_dpi.cpp")
    if (args(0) == "--check") {
      require(new String(Files.readAllBytes(source), UTF_8) == expected, "generated Storage DPI C++ is stale")
      println(s"Validated ${models.size} storage descriptors and generated DPI C++")
    } else {
      Files.write(source, expected.getBytes(UTF_8))
      println(s"Generated DPI C++ from ${models.size} storage descriptors")
    }
  }
}

sealed trait StorageBackend
object StorageBackend {
  case object RTL extends StorageBackend
  final case class CoSim(readBridge: StorageReadBridgeMode = StorageReadBridgeMode.PerRead) extends StorageBackend

  def parse(value: String): StorageBackend = value match {
    case "rtl" => RTL
    case "cosim" => CoSim()
    case _ => throw new IllegalArgumentException("Storage backend must be rtl or cosim")
  }
}

/** One option for all Storage declarations in an elaboration. */
final case class StorageBackendAnnotation(
  backend: StorageBackend,
  runtimeHeader: String = "src/test/scala/csrc/storage/storage.hpp"
) extends NoTargetAnnotation with Unserializable

object StorageBackendAnnotation extends HasShellOptions {
  val options = Seq(new ShellOption[String](
    longOption = "storage-backend",
    toAnnotationSeq = value => Seq(StorageBackendAnnotation(StorageBackend.parse(value))),
    helpText = "Select the backend for every Storage declaration (default: rtl)",
    helpValueName = Some("rtl|cosim")
  ))
}

object StorageModelStage {
  private val defaultRuntime = Paths.get("src/test/scala/csrc/storage/storage.hpp")

  /** Generate the same module source for either target. The stage owns the
    * backend scope, session, instance IDs and software artifact emission.
    */
  def emit(
    generator: () => RawModule,
    buildDirectory: Path,
    backend: StorageBackend = StorageBackend.RTL,
    runtimeHeader: Path = defaultRuntime
  ): Vector[StorageIR] = {
    val build = buildDirectory.toAbsolutePath.normalize()
    withBackend(StorageBackendAnnotation(backend, runtimeHeader.toString), build) { session =>
      (new top.XiangShanStage).execute(
        Array("--target", "systemverilog", "--target-dir", build.toString),
        Seq(ChiselGeneratorAnnotation(generator), FirtoolOption("--split-verilog"),
          FirtoolOption(s"-o=${build.resolve("rtl")}")))
      session.models
    }
  }

  private[IntentionModel] def artifacts(models: Vector[StorageIR], build: Path, runtimeHeader: Path): Unit = {
    val generated = build.resolve("generated-src")
    StorageDPIEmitter.emit(models, generated)
    Files.copy(runtimeHeader, generated.resolve("storage.hpp"), StandardCopyOption.REPLACE_EXISTING)
  }

  /** Called around the complete stage pipeline so conversion seals the same
    * session that allocated all nested storage instances.
    */
  def withBackend[A](option: StorageBackendAnnotation, build: Path)(generate: StorageIRSession => A): A = {
    val session = new StorageIRSession
    val implementation = option.backend match {
      case StorageBackend.RTL => Storage.RTL
      case StorageBackend.CoSim(readBridge) =>
        require(Files.isRegularFile(Paths.get(option.runtimeHeader)),
          s"Storage runtime header is missing: ${option.runtimeHeader}")
        new Storage.Backend {
          def apply[T <: Data](gen: T, entries: Int, initial: Option[Seq[T]]): Storage[T] =
            new ModelStorage(gen, entries, initial, readBridge)(session)
        }
    }
    val result = Storage.withBackend(implementation)(generate(session))
    if (option.backend != StorageBackend.RTL) artifacts(session.models, build, Paths.get(option.runtimeHeader))
    result
  }

  /** Compatibility entry point for explicitly instantiated ModelStorage. */
  def emit(generator: StorageIRSession => RawModule, buildDirectory: Path): Vector[StorageIR] =
    emit(generator, buildDirectory, defaultRuntime)

  def emit(
    generator: StorageIRSession => RawModule,
    buildDirectory: Path,
    runtimeHeader: Path
  ): Vector[StorageIR] = {
    require(Files.isRegularFile(runtimeHeader), s"Storage runtime header is missing: $runtimeHeader")
    val build = buildDirectory.toAbsolutePath.normalize()
    val session = new StorageIRSession
    (new top.XiangShanStage).execute(
      Array("--target", "systemverilog", "--target-dir", build.resolve(".storage-stage").toString),
      Seq(ChiselGeneratorAnnotation(() => generator(session)), FirtoolOption("--split-verilog"),
        FirtoolOption(s"-o=${build.resolve("rtl")}")))
    val models = session.models
    artifacts(models, build, runtimeHeader)
    models
  }
}
