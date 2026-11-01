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
  * Enqueue accepts a per-port acceptance mask and appends only accepted
  * elements, preserving port order. Dequeue consumes a head prefix. Callers
  * own capacity/ownership legality; this core does not clamp either transfer.
  *
  * Core priority, in decreasing order:
  *   1. `flush` clears membership at the edge; current-cycle outputs still
  *      describe old state, and admission is suppressed in that cycle.
  *   2. `recover` installs a target tail and suppresses enqueue, even when the
  *      target equals the old tail; dequeue proceeds independently.
  *   3. Core enqueue and dequeue commit together. The last `enq` registration
  *      owns the whole accepted batch. Its pointer step and payload writes are
  *      emitted together after element updates, so a full-entry replacement
  *      always wins a recycled-slot RMW and a final empty mask suppresses
  *      every earlier batch.
  *
  * Update functions are assumed pure and accumulated by ordered composition.
  * The caller explicitly expands the composite with `emitRTL` after completing
  * construction; read-only observers expand at their call sites. Both receive
  * current payload at every physical slot, including unallocated slots, without
  * implicit guards or slot arguments. Callers supply validity guards. Purity
  * and ambient construction context are not inspected or captured.
  */
abstract class RingQueue[T <: Data](
  gen: T,
  entries: Int,
  val numEnq: Int,
  val numDeq: Int
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends QueueBase[T](entries) with QueueStatus {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")
  require(numEnq > 0, "Queue needs at least one enqueue port")
  require(numDeq > 0, "Queue needs at least one dequeue port")

  /** Evaluated during superclass construction. Partial literals reset only
    * specified fields; other payload fields remain unreset, including writes
    * concurrent with reset. Contents do not determine pointer membership.
    */
  protected def initContents: Option[Seq[T]] = None

  private val contents: Vec[T] = initContents match {
    case Some(initial) =>
      require(initial.length == entries, "initial contents must describe exactly one entry per slot")
      RegInit(VecInit(initial))
    case None => Reg(Vec(entries, gen))
  }
  private var modifyElement: T => Unit = _ => ()
  private var emitted = false

  protected final def compose(first: T => Unit, second: T => Unit): T => Unit =
    entry => { first(entry); second(entry) }

  override protected final def foreach(read: T => Unit): Unit = contents.foreach(read)

  /** Conditions must be inside the callback: ambient Chisel when scopes are
    * not captured for later expansion. Field-wise last-enabled-write priority
    * applies; every right-hand-side expression still reads the old entry.
    */
  override protected final def update(modify: T => Unit): Unit = {
    require(!emitted, "element updates must be registered before emission")
    modifyElement = compose(modifyElement, modify)
  }

  override protected final def exists(predicate: T => Bool): Bool = {
    val result = WireDefault(false.B)
    foreach { entry => when(predicate(entry)) { result := true.B } }
    result
  }

  /** Call once after all inherited and concrete update registrations, while
    * constructing the owning Module. This does not gate writes on reset.
    */
  private def emitElementUpdates(): Unit = {
    require(!emitted, "element updates must be emitted exactly once")
    emitted = true
    foreach(modifyElement)
  }

  /** Elaboration-time slot identity for generic QueueModel circuit lowering.
    * This returns neither entry state nor an addressable storage handle. Entry
    * observations and writes still occur exclusively in foreach/update bodies.
    * External specializations can carry an immutable slot field in their entry.
    */
  private[QueueModel] final def physicalSlot(entry: T): Int = {
    val slot = contents.indexWhere(_ eq entry)
    require(slot >= 0, "slot identity requires this Queue's callback entry")
    slot
  }

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

  /** Append accepted elements in port order, without holes in membership.
    * Acceptance, including capacity and handshake decisions, belongs to the
    * caller. Each accepted port writes at tail + its accepted-prefix count;
    * data stays on its original port instead of crossing a separate packer.
    * Under the capacity contract the destinations are distinct. If a caller
    * violates that contract, later ports retain last-enabled-write priority.
    *
    * The latest call replaces the whole saved batch, including an empty batch.
    * Its payload writes override earlier element updates at recycled slots,
    * and its pointer step commits at the same edge. Flush/recovery suppress
    * both effects. Destination arithmetic is shared outside the entry loop.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit = {
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    enqueueBatch = () => {
      when(!recoverWire && !flushWire && VecInit(accepted).asUInt.orR) {
        enqPtr := enqPtr + PopCount(accepted)
      }
      for (i <- elements.indices) {
        val destination = (enqPtr + PopCount(accepted.take(i))).value
        update { entry =>
          when(!recoverWire && !flushWire && accepted(i) && destination === physicalSlot(entry).U) {
            entry := elements(i)
          }
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
      deqPtr := deqPtr + count
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
      enqPtr := target
    }
  }

  /** Synchronous clear. Outputs continue to observe old state in this cycle;
    * flush wins over enqueue, recovery and dequeue advancement at the edge.
    */
  protected def flush(enable: Bool): Unit = {
    flushWire := enable
    when(enable) {
      enqPtr := 0.U.asTypeOf(new QueuePtr)
      deqPtr := 0.U.asTypeOf(new QueuePtr)
    }
  }

}
