// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.Data._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.Annotation
import scala.collection.mutable.ArrayBuffer
import utils.QueueModel.{FieldOf, Storage}

/** Directly instantiated software storage within a StorageModelOwner module.
  * StorageModelStage emits RTL and descriptors; RegStorage remains the default.
  */
class ModelStorage[T <: Data](
  gen: T, entries: Int, initContents: Option[Seq[T]] = None,
  readBridge: StorageReadBridgeMode = StorageReadBridgeMode.PerRead
)(
  implicit session: StorageIRSession
) extends Storage[T](entries) {
  private val owner = Module.currentModule match {
    case Some(module: StorageModelOwner) => module
    case _ => throw new IllegalArgumentException("ModelStorage requires a StorageModelOwner module")
  }
  private val clock = Module.clock
  private val reset = Module.reset
  require(reset.isInstanceOf[Bool], "ModelStorage requires a synchronous Bool reset")
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

  owner.registerStorage(() => {
    val ports = builder.writes
    val widths = ports.map(port => builder.data(port.index).getWidth -> builder.data(port.value).getWidth)
    val cycle = Module(new StorageCycleBridge(builder.id, widths, initial.isInstanceOf[ConstantInitialState]))
    cycle.io.clock := clock
    cycle.io.reset := reset.asBool
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
