// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3.{Data, UInt}
import scala.collection.mutable.ArrayBuffer

/** Explicit per-elaboration ownership: IDs restart in a new session. */
final class StorageIRSession {
  private var nextId = 0L
  private val artifacts = ArrayBuffer.empty[StorageIR]
  private[IntentionModel] def allocate(): StorageId = {
    val id = StorageId(nextId)
    nextId = Math.addExact(nextId, 1L)
    id
  }
  private[IntentionModel] def record(ir: StorageIR): Unit = {
    require(!artifacts.exists(_.id == ir.id), "duplicate storage instance ID")
    artifacts += ir
  }
  def models: Vector[StorageIR] = artifacts.toVector
}

private[IntentionModel] final case class StorageWritePort(
  id: OpId, guard: ValueId, index: ValueId, value: ValueId, offset: Int, width: Int
)

/** Data identities live only here and are released after Stage sealing. */
final class StorageIRBuilder(
  val id: StorageId, val entries: Int, val layout: EntryLayout, val initial: InitialState
) {
  private final case class Captured(id: ValueId, data: Data, tpe: StorageType, read: Option[OpId])
  private val captured = ArrayBuffer.empty[Captured]
  private val ops = ArrayBuffer.empty[StorageOp]
  private val ports = ArrayBuffer.empty[StorageWritePort]
  private var closed = false
  private var sealedIR: Option[StorageIR] = None
  private def open(): Unit = require(!closed && sealedIR.isEmpty, "storage capture is closed")
  def isSealed: Boolean = sealedIR.nonEmpty
  private[IntentionModel] def nextOp: OpId = { open(); OpId(ops.size) }
  private[IntentionModel] def capture(data: Data, tpe: StorageType, read: Option[OpId] = None): ValueId = {
    open()
    require(data.getWidth == StorageLayout.width(tpe), "captured value width mismatch")
    val value = ValueId(captured.size)
    captured += Captured(value, data, tpe, read)
    value
  }
  private[IntentionModel] def read(index: ValueId, data: Data, source: SourceLoc): (OpId, ValueId) = {
    val op = nextOp
    val result = capture(data, layout.shape, Some(op))
    ops += ReadOp(op, ops.size, Operand(index), result, source)
    (op, result)
  }
  private[IntentionModel] def write(
    guard: ValueId, index: ValueId, value: ValueId, path: Option[Vector[FieldStep]], source: SourceLoc
  ): Unit = {
    val op = nextOp
    val (_, offset, width) = path.map(StorageLayout.field(layout, _)).getOrElse((layout.shape, 0, layout.totalWidth))
    ops += (path match {
      case Some(p) => WriteFieldOp(op, ops.size, Operand(guard), Operand(index), p, Operand(value), source)
      case None => WriteOp(op, ops.size, Operand(guard), Operand(index), Operand(value), source)
    })
    ports += StorageWritePort(op, guard, index, value, offset, width)
  }
  private[IntentionModel] def writes: Vector[StorageWritePort] = ports.toVector
  private[IntentionModel] def data(id: ValueId): Data = captured(id.value).data
  private[IntentionModel] def closeCapture(): Unit = { open(); closed = true }

  /** Called explicitly by the Stage annotation conversion after elaboration. */
  def seal(
    instancePath: String, clock: SignalRef, reset: ResetSpec, resolve: Data => SignalRef
  ): StorageIR = sealedIR.getOrElse {
    require(closed, "close capture before sealing")
    val values = captured.map { c =>
      val binding = c.read match {
        case Some(op) => ReadResultValue(op, resolve(c.data))
        case None => c.data.litOption match {
          case Some(bits) => ConstantValue(bits & StorageLayout.mask(StorageLayout.width(c.tpe)))
          case None => SignalValue(resolve(c.data))
        }
      }
      StorageValue(c.id, c.tpe, binding)
    }.toVector
    val ir = StorageIR(1, id, Some(instancePath), entries, layout, clock, reset, initial,
      values, Vector.empty, ops.toVector)
    StorageIRValidation.requireDPI(ir)
    sealedIR = Some(ir)
    captured.clear()
    ops.clear()
    ports.clear()
    ir
  }
}
