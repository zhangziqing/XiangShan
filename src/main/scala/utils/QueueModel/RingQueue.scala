// SPDX-License-Identifier: MulanPSL-2.0
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

trait Flushable {
  def flush: Bool
}

trait SpeculativeRecovery[R <: Data] {
  def recover: ValidIO[R]
}

trait QueueStatusIO {
  def count: UInt
  def empty: Bool
  def full: Bool
}

/** Shared dequeue and status ports; subclasses choose the enqueue protocol. */
abstract class QueueBaseIO[T <: Data](gen: T, entries: Int, numDeq: Int)
    extends Bundle with QueueDequeue[T] with QueueStatusIO {
  val deq = Vec(numDeq, Decoupled(gen))
  val count = Output(UInt(log2Ceil(entries + 1).W))
  val empty = Output(Bool())
  val full = Output(Bool())
}

/** FIFO storage with live entries in `[deqPtr, enqPtr)`.
  * Subclasses define IO and register updates before calling `emitRTL`.
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

  /** Override to reset payload contents; the default leaves them unreset.
    * Evaluated during superclass construction, before subclass fields initialize.
    * Partial literals reset only the specified fields.
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

  /** Put Chisel conditions inside the callback; it runs later in `emitRTL`. */
  override protected final def update(modify: T => Unit): Unit = {
    require(!emitted, "element updates must be registered before emission")
    modifyElement = compose(modifyElement, modify)
  }

  override protected final def exists(predicate: T => Bool): Bool = {
    val result = WireDefault(false.B)
    foreach { entry => when(predicate(entry)) { result := true.B } }
    result
  }

  private def emitElementUpdates(): Unit = {
    require(!emitted, "element updates must be emitted exactly once")
    emitted = true
    foreach(modifyElement)
  }

  /** Find a callback entry's slot index while generating the circuit. */
  private[QueueModel] final def physicalSlot(entry: T): Int = {
    val slot = contents.indexWhere(_ eq entry)
    require(slot >= 0, "slot identity requires this Queue's callback entry")
    slot
  }

  private val recoverWire = WireDefault(false.B)
  private val flushWire = WireDefault(false.B)

  private var enqConfig: () => Unit = () => ()

  protected final def flushActive: Bool = flushWire

  /** Read the oldest `numDeq` slots; payload is unspecified beyond occupancy. */
  protected final val deqData: Seq[T] = (0 until numDeq).map { offset =>
    val index = (deqPtr + offset.U).value
    val choices = scala.collection.mutable.ArrayBuffer.empty[(Bool, T)]
    foreach { entry => choices += ((index === physicalSlot(entry).U) -> entry) }
    Mux1H(choices.toSeq)
  }

  /** Emit registered updates once, with enqueue writes taking priority.
    * Call after the subclass has registered its updates and transfers.
    *
    * Keep emission inside Module construction, e.g.
    * `Module(new SomeQueue(...).emitRTL())`.
    */
  final def emitRTL(): this.type = {
    enqConfig()
    emitElementUpdates()
    this
  }

  /** Accept all valid inputs together if they fit, using space before dequeue.
    * Overrides must return one decision per input and reject invalid ports.
    */
  protected def accept(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val canAccept = (count +& PopCount(elements.map(_.valid))) <= entries.U
    elements.map(element => element.valid && canAccept && !recoverWire && !flushWire)
  }

  /** Register Valid inputs and return their actual acceptance for this cycle.
    * Flush or recovery forces false. A later `enq` call cancels this registration,
    * leaving its result false.
    */
  protected def enq(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    val accepted = Seq.fill(elements.length)(WireDefault(false.B))
    enqConfig = () => {
      val result = enqImpl(elements.map(_.bits), accept(elements))
      accepted.zip(result).foreach { case (out, in) => out := in }
    }
    accepted
  }

  /** Append accepted ports in order; the caller is responsible for capacity.
    * Only the last `enq` call takes effect, even if every acceptance bit is false.
    *
    * Enqueue writes override element updates. Flush or recovery suppresses
    * both the writes and tail advance.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit = {
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    enqConfig = () => enqImpl(elements, accepted)
  }

  private def enqImpl(elements: Seq[T], accepted: Seq[Bool]): Seq[Bool] = {
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    when(!recoverWire && !flushWire && VecInit(accepted).asUInt.orR) {
      enqPtr := enqPtr + PopCount(accepted)
    }
    for (i <- elements.indices) {
      // Share each port's destination calculation across all physical slots.
      val destination = (enqPtr + PopCount(accepted.take(i))).value
      update { entry =>
        when(!recoverWire && !flushWire && accepted(i) && destination === physicalSlot(entry).U) {
          entry := elements(i)
        }
      }
    }
    accepted.map(_ && !recoverWire && !flushWire)
  }

  /** Consume the oldest `count` entries by advancing the head.
    * The caller must keep count within current occupancy.
    */
  protected def deq(count: UInt): Unit = {
    when(!flushWire) {
      deqPtr := deqPtr + count
    }
  }

  /** Restore the tail to discard newer entries, suppressing enqueue.
    * The target must lie between the surviving head and old tail.
    * Dequeue still proceeds during recovery.
    */
  protected def recover(target: QueuePtr, enable: Bool): Unit = {
    recoverWire := enable
    when(enable && !flushWire) {
      enqPtr := target
    }
  }

  /** Empty the queue at the next edge, overriding other pointer updates.
    * Current-cycle reads still see the old state.
    */
  protected def flush(enable: Bool): Unit = {
    flushWire := enable
    when(enable) {
      enqPtr := 0.U.asTypeOf(new QueuePtr)
      deqPtr := 0.U.asTypeOf(new QueuePtr)
    }
  }

}
