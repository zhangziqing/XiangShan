// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.Data._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

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

  /** Read current storage without exposing an assignment destination. */
  protected final def read(index: Int): T = contents(index).readOnly
  protected final def read(index: UInt): T = contents(index).readOnly

  protected final def compose(first: T => Unit, second: T => Unit): T => Unit =
    entry => { first(entry); second(entry) }

  override protected final def foreach(read: T => Unit): Unit = contents.foreach(entry => read(entry.readOnly))

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

  private def emitUpdates(): Unit = {
    require(!emitted, "element updates must be emitted exactly once")
    emitted = true
    contents.foreach(modifyElement)
  }

  /** Find a callback entry's slot index while generating the circuit. */
  private[QueueModel] final def physicalSlot(entry: T): Int = {
    val slot = contents.indexWhere(_ eq entry)
    require(slot >= 0, "slot identity requires this Queue's callback entry")
    slot
  }

  /** Flush clears both pointers; recovery restores an exclusive tail.
    * Recovery suppresses enqueue, while dequeue proceeds. Set recoverPtr to a
    * target between the surviving head and old tail. Unconnected controls are
    * inactive; current-cycle reads still observe old state.
    */
  protected val flush: Bool = WireDefault(false.B)
  protected val recover: Bool = WireDefault(false.B)
  protected val recoverPtr: QueuePtr = WireDefault(enqPtr)

  when(recover && !flush) {
    enqPtr := recoverPtr
  }
  when(flush) {
    enqPtr := 0.U.asTypeOf(new QueuePtr)
    deqPtr := 0.U.asTypeOf(new QueuePtr)
  }

  private var enqCallback: () => Unit = () => ()

  /** Emit registered updates once, with enqueue writes taking priority.
    * Call after the subclass has registered its updates and transfers.
    *
    * Keep emission inside Module construction, e.g.
    * `Module(new SomeQueue(...).emitRTL())`.
    */
  final def emitRTL(): this.type = {
    enqCallback()
    emitUpdates()
    this
  }

  /** Accept all valid inputs together if they fit, using space before dequeue.
    * Overrides must return one decision per input and reject invalid ports,
    * flush and recovery.
    */
  protected def accept(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val canAccept = (count +& PopCount(elements.map(_.valid))) <= entries.U
    elements.map(element => element.valid && canAccept && !recover && !flush)
  }

  /** Return this batch's admission decision and register its enqueue.
    * A later `enq` replaces the writes without cancelling this decision.
    */
  protected def enq(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val accepted = accept(elements)
    enq(elements.map(_.bits), accepted)
    accepted
  }

  /** Place accepted ports in order at the writer. Callers own membership and
    * replacement policy; dense FIFO callers must respect available capacity.
    * Only the last `enq` call takes effect, even if every acceptance bit is false.
    *
    * Enqueue writes override element updates. Flush or recovery suppresses
    * both the writes and tail advance.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit = {
    require(!emitted, "enqueue must be registered before emission")
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    enqCallback = () => enqImpl(elements, accepted)
  }

  private def enqImpl(elements: Seq[T], accepted: Seq[Bool]): Unit = {
    when(!recover && !flush && VecInit(accepted).asUInt.orR) {
      enqPtr := enqPtr + PopCount(accepted)
    }
    for (i <- elements.indices) {
      // Share each port's destination calculation across all physical slots.
      val destination = (enqPtr + PopCount(accepted.take(i))).value
      update { entry =>
        when(!recover && !flush && accepted(i) && destination === physicalSlot(entry).U) {
          entry := elements(i)
        }
      }
    }
  }

  /** Read current head-relative candidates and consume `count` entries.
    * Reads remain visible when count is zero; payload beyond occupancy is
    * unspecified. The caller must keep count within current occupancy.
    */
  protected def deq(count: UInt, offsets: Seq[UInt] = (0 until numDeq).map(_.U)): Seq[T] = {
    require(offsets.nonEmpty && offsets.length <= numDeq, "dequeue exceeds the configured read window")
    advanceDeq(count)
    offsets.map(offset => read((deqPtr + offset).value))
  }

  /** Advance consumption without reading, for callers with separate previews. */
  protected final def advanceDeq(count: UInt): Unit = {
    when(!flush && count.orR) {
      deqPtr := deqPtr + count
    }
  }
}
