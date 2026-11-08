// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.Data._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility.OneHot.UIntToOHSeq

/** FIFO storage with live entries in `[deqPtr, enqPtr)`. */
abstract class RingQueue[T <: Data](
  gen: T,
  entries: Int,
  val numEnq: Int,
  val numDeq: Int
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends QueueBase[T](entries) with QueueStatus {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")
  require(numEnq > 0, "Queue needs at least one enqueue port")
  require(numDeq > 0, "Queue needs at least one dequeue port")

  /** Runs before subclass fields initialize; partial literals reset only specified fields. */
  protected def initContents: Option[Seq[T]] = None

  private val contents: Vec[T] = initContents match {
    case Some(initial) =>
      require(initial.length == entries, "initial contents must describe exactly one entry per slot")
      RegInit(VecInit(initial))
    case None => Reg(Vec(entries, gen))
  }
  private var modifyElement: T => Unit = _ => ()
  private var emitted = false

  override protected final def read(index: Int): T = contents(index).readOnly
  override protected final def read(index: UInt): T = Mux1H(UIntToOHSeq(index, entries), contents.toSeq).readOnly

  protected final def compose(first: T => Unit, second: T => Unit): T => Unit =
    entry => { first(entry); second(entry) }

  override protected final def foreach(read: T => Unit): Unit = contents.foreach(entry => read(entry.readOnly))

  private def requireRegistrationScope(): Unit = {
    require(!emitted, "modifications must be registered before emission")
    require(when.cond.litOption.contains(BigInt(1)),
      "register delayed Queue operations outside when with explicit enables")
  }

  override protected final def update(modify: T => Unit): Unit = {
    requireRegistrationScope()
    modifyElement = compose(modifyElement, modify)
  }

  /** Register outside when; flush/recovery do not suppress indexed writes. */
  override protected final def write(index: UInt, data: T, enable: Bool): Unit =
    writeField(index, (entry: T) => entry, data, enable)

  override protected final def writeField[D <: Data](
    index: UInt, field: T => D, data: D, enable: Bool
  ): Unit = {
    requireRegistrationScope()
    val selected = UIntToOHSeq(index, entries)
    update { entry =>
      when(enable && selected(physicalSlot(entry))) { field(entry) := data }
    }
  }

  override protected final def exists(predicate: T => Bool): Bool = {
    val result = WireDefault(false.B)
    foreach { entry => when(predicate(entry)) { result := true.B } }
    result
  }

  private def emitUpdates(): Unit = {
    requireRegistrationScope()
    emitted = true
    contents.foreach(modifyElement)
  }

  /** Uses callback object identity, not payload equality. */
  private[QueueModel] final def physicalSlot(entry: T): Int = {
    val slot = contents.indexWhere(_ eq entry)
    slot
  }

  /** Flush takes priority over recovery, which blocks enqueue but permits dequeue.
    * recoverPtr is an exclusive tail between the surviving head and old tail.
    */
  protected val flush: Bool = WireDefault(false.B)
  protected val recover: Bool = WireDefault(false.B)
  protected val recoverPtr: QueuePtr = WireDefault(enqPtr)

  when(recover && !flush) { onRecover() }
  when(flush) { onFlush() }

  // Both hooks run during superclass construction, before subclass fields initialize.
  override protected def onRecover(): Unit = {
    enqPtr := recoverPtr
  }

  override protected def onFlush(): Unit = {
    enqPtr := 0.U.asTypeOf(new QueuePtr)
    deqPtr := 0.U.asTypeOf(new QueuePtr)
  }

  private var enqDefinition: Option[(Seq[T], Seq[Bool], Option[Seq[UInt]])] = None

  /** Call once after registration, inside Module construction. */
  final def emitRTL(): this.type = {
    val (elements, accepted, offsets) = enqDefinition.getOrElse {
      throw new IllegalArgumentException("enqueue must be registered before emission")
    }
    emitUpdates()
    emitEnq(elements, accepted, offsets)
    this
  }

  /** Default admission accepts the whole valid batch without dequeue credit. */
  override protected def enqGuard(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val canAccept = (count +& PopCount(elements.map(_.valid))) <= entries.U
    elements.map(element => element.valid && canAccept && !recover && !flush)
  }

  override protected def enq(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val accepted = enqGuard(elements)
    enq(elements.map(_.bits), accepted)
    accepted
  }

  override protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit =
    registerEnq(elements, accepted, None)

  /** Register accepted inputs at explicit tail-relative writer offsets.
    * Only `accepted(i)` writes, at `(enqPtr + offsets(i)).value`; an enabled
    * offset must address a physical slot in `[0, entries)`. Aliased offsets
    * write in port order, so the later port wins. The tail still advances by
    * the accepted count. Offsets need not be a packed prefix, so a concrete
    * owner can preserve fixed physical lanes. The two-argument enqueue above
    * remains the default packed placement.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool], offsets: Seq[UInt]): Unit =
    registerEnq(elements, accepted, Some(offsets))

  private def registerEnq(
    elements: Seq[T], accepted: Seq[Bool], offsets: Option[Seq[UInt]]
  ): Unit = {
    requireRegistrationScope()
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    require(offsets.forall(_.length == elements.length), "enqueue data and offset widths must match")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    enqDefinition = Some((elements, accepted, offsets))
  }

  private def emitEnq(
    elements: Seq[T], accepted: Seq[Bool], offsets: Option[Seq[UInt]]
  ): Unit = {
    when(!recover && !flush) {
      for (i <- elements.indices) {
        when(accepted(i)) {
          val offset = offsets.map(_(i)).getOrElse(PopCount(accepted.take(i)))
          contents((enqPtr + offset).value) := elements(i)
        }
      }
    }
    advanceEnq(PopCount(accepted))
  }

  /** No default occupancy check. */
  override protected def deqGuard(count: UInt): Bool = !flush

  override protected def deq(count: UInt, offsets: Seq[UInt] = (0 until numDeq).map(_.U)): Seq[T] = {
    require(offsets.nonEmpty && offsets.length <= numDeq, "dequeue exceeds the configured read window")
    when(deqGuard(count)) { advanceDeq(count) }
    offsets.map(offset => read((deqPtr + offset).value))
  }

  override protected final def advanceDeq(count: UInt): Unit = {
    when(!flush && count.orR) {
      deqPtr := deqPtr + count
    }
  }

  override protected final def advanceEnq(count: UInt): Unit = {
    when(!flush && !recover && count.orR) {
      enqPtr := enqPtr + count
    }
  }
}
