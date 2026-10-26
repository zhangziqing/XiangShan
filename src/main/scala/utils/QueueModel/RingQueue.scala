// SPDX-License-Identifier: MulanPSL-2.0
// Dense circular Queue core with element-oriented extension callbacks.
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

trait DecoupledEnqueue[T <: Data] {
  def enq: Vec[DecoupledIO[T]]
}

trait ValidEnqueue[T <: Data] {
  def enq: Vec[ValidIO[T]]
}

trait QueueDequeue[T <: Data] {
  def deq: Vec[DecoupledIO[T]]
}

/** An existing input port; this trait allocates neither IO nor state. */
trait Flushable {
  def flush: Bool
}

/** Request shape belongs to the concrete interface, not an internal specuPtr. */
trait SpeculativeRecovery[R <: Data] {
  def recover: ValidIO[R]
}

/** IO observation fields; computation belongs to the QueueStatus mixin. */
trait QueueStatusIO {
  def count: UInt
  def empty: Bool
  def full: Bool
}

/** The common IO deliberately leaves the enqueue protocol to the concrete IO. */
abstract class QueueBaseIO[T <: Data](gen: T, entries: Int, numDeq: Int)
    extends Bundle with QueueDequeue[T] with QueueStatusIO {
  val deq = Vec(numDeq, Decoupled(gen))
  val count = Output(UInt(log2Ceil(entries + 1).W))
  val empty = Output(Bool())
  val full = Output(Bool())
}

/** A payload-bearing dense Queue owns its state, control and element extensions.
  *
  * Membership is exactly the circular interval `[deqPtr, enqPtr)`; it has no
  * holes and no independent occupancy register. Pointers carry unsigned value
  * bits plus one wrap flag, so non-power-of-two capacities wrap without binary
  * truncation. `count`, `empty` and `full` are derived from the pointers.
  *
  * Pointer reset values come from `enqPtrInit` and `deqPtrInit`. Payload
  * registers are deliberately unreset unless a concrete specialization
  * overrides `initContents`. Partial literals may reset only entry metadata;
  * initializing contents does not change the pointer interval.
  *
  * Core priority, in decreasing order:
  *   1. `flush` clears membership at the edge; current-cycle outputs still
  *      describe old state, and admission is suppressed in that cycle.
  *   2. `recover` installs a target tail and suppresses enqueue, even when the
  *      target equals the old tail; dequeue proceeds independently.
  *   3. Core enqueue and dequeue commit together. The last `enq` registration
  *      owns the whole accepted batch. Its pointer step and payload writes are
  *      emitted together after element updates, so a full-entry replacement
  *      always wins a recycled-slot RMW and a final count of zero suppresses
  *      every earlier batch.
  *
  * Update functions are assumed pure and accumulated by ordered composition.
  * The caller explicitly expands the composite with `emitRTL` after completing
  * construction; read-only observers expand at their call sites. Both receive
  * current payload without implicit guards or slot arguments. Purity and
  * ambient construction context are not inspected or captured.
  */
abstract class RingQueue[T <: Data](
  gen: T,
  entries: Int,
  val numEnq: Int,
  val numDeq: Int
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends ElementQueue[T](gen, entries) with QueueStatus {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")
  require(numEnq > 0, "Queue needs at least one enqueue port")
  require(numDeq > 0, "Queue needs at least one dequeue port")

  private val recoverWire = WireDefault(false.B)
  private val flushWire = WireDefault(false.B)

  private var enqueueBatch: () => Unit = () => ()

  protected final def flushActive: Bool = flushWire

  /** Ordered head window, fixed by the dequeue width. Invalid lanes have no
    * payload guarantee. This does not provide arbitrary-address access.
    */
  protected final val deqData: Seq[T] = (0 until numDeq).map { offset =>
    val index = (deqPtr + offset.U).value
    val choices = scala.collection.mutable.ArrayBuffer.empty[(Bool, T)]
    foreach { entry => choices += ((index === physicalSlot(entry).U) -> entry) }
    Mux1H(choices.toSeq)
  }

  /** Call once after all inherited and concrete updates/transfers are defined,
    * while this Queue is still being constructed inside `Module(...)`:
    * `Module(new SomeQueue(...).emitRTL())`.
    *
    * Expansion orders element updates before the final enqueue batch. Returning
    * this instance keeps generation inside the owning Module's construction
    * expression, before Chisel closes it.
    */
  final def emitRTL(): this.type = {
    enqueueBatch()
    emitElementUpdates()
    this
  }

  /** Compact accepted inputs into fixed destination indices, in port order. */
  protected def packEnqueue(elements: Seq[T], accepted: Seq[Bool]): Seq[T] = {
    val packed = Wire(Vec(elements.length, gen))
    packed := 0.U.asTypeOf(packed)
    val ranks = elements.indices.map(j => PopCount(accepted.take(j)))
    for (i <- elements.indices; j <- elements.indices) {
      when(accepted(j) && ranks(j) === i.U) { packed(i) := elements(j) }
    }
    packed.toSeq
  }

  /** Enqueue a packed prefix of `elements`. `count` is the hardware number of
    * leading elements to accept, bounded by the Scala sequence length. The
    * core is guarded by recovery and flush; callers own admission and legal
    * count bounds. Transfer conditions are described by these hardware values.
    * The latest call replaces the whole saved batch, so pointer advancement and
    * payload writes belong to that final call; a later count of zero therefore
    * suppresses every earlier payload write as well as the pointer step.
    * Each port's destination is decoded once outside the slot loop; each slot
    * then builds a one-hot port selector whose orR is its write enable and whose
    * Mux1H selects the replacement payload. Both effects are emitted together
    * for `emitRTL`.
    */
  protected def enq(elements: Seq[T], count: UInt): Unit = {
    enqueueBatch = () => {
      when(!recoverWire && !flushWire) { enqPtrReg := enqPtr + count }
      // Decode destinations once; the final batch owns each recycled entry.
      val valid = elements.indices.map(i => i.U < count)
      val destOH = elements.indices.map(i => UIntToOH((enqPtr + i.U).value, entries))
      update { entry =>
        val slot = physicalSlot(entry)
        val writeMask = elements.indices.map(i => valid(i) && destOH(i)(slot))
        val writeData = Mux1H(writeMask, elements)
        when(!recoverWire && !flushWire && VecInit(writeMask).asUInt.orR) {
          entry := writeData
        }
      }
    }
  }

  /** Remove `count` oldest entries. Payload is read separately through the
    * pointer or dequeue ports before the edge; removal itself only advances the
    * head. The count must not exceed current occupancy.
    */
  protected def deq(count: UInt): Unit = {
    when(!flushWire) {
      deqPtrReg := deqPtr + count
    }
  }

  /** Restore the exclusive tail position, including its circular phase.
    * Recovery suppresses enqueue even when the target is unchanged; dequeue
    * proceeds independently. The caller places the target between the surviving
    * head and old tail, preserving a dense interval of at most `entries`.
    */
  protected def recover(target: QueuePtr, enable: Bool): Unit = {
    recoverWire := enable
    when(enable && !flushWire) {
      enqPtrReg := target
    }
  }

  /** Synchronous clear. Outputs continue to observe old state in this cycle;
    * flush wins over enqueue, recovery and dequeue advancement at the edge.
    */
  protected def flush(enable: Bool): Unit = {
    flushWire := enable
    when(enable) {
      enqPtrReg := 0.U.asTypeOf(new QueuePtr)
      deqPtrReg := 0.U.asTypeOf(new QueuePtr)
    }
  }

}
