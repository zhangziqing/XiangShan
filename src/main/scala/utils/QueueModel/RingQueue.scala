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

  override protected final def read(index: Int): T = contents(index).readOnly
  override protected final def read(index: UInt): T = Mux1H(UIntToOHSeq(index, entries), contents.toSeq).readOnly

  protected final def compose(first: T => Unit, second: T => Unit): T => Unit =
    entry => { first(entry); second(entry) }

  override protected final def foreach(read: T => Unit): Unit = contents.foreach(entry => read(entry.readOnly))

  override protected final def update(modify: T => Unit): Unit = contents.foreach(modify)

  /** Flush/recovery do not suppress indexed writes. */
  override protected final def write(index: UInt, data: T, enable: Bool): Unit =
    writeField(index, (entry: T) => entry, data, enable)

  override protected final def writeField[D <: Data](
    index: UInt, field: T => D, data: D, enable: Bool
  ): Unit = {
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

  /** Compatibility alias; operations generate hardware when called. */
  final def emitRTL(): this.type = this

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

  override protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit = {
    require(elements.nonEmpty, "enqueue needs at least one input")
    require(elements.length == accepted.length, "enqueue data and acceptance widths must match")
    require(elements.length <= numEnq, "enqueue exceeds the configured port count")
    when(!recover && !flush) {
      for (i <- elements.indices) {
        when(accepted(i)) {
          val offset = PopCount(accepted.take(i))
          contents((enqPtr + offset).value) := elements(i)
        }
      }
    }
    advanceEnq(PopCount(accepted))
  }

  /** Reservation enqueue: accepted payload fills absolute circular ranges, with
    * lowest-port priority on overlap. Reservation is independent of acceptance;
    * recovery replaces the tail step but does not suppress these payload writes.
    * The guard owns range admission; flush still suppresses payload writes.
    */
  protected final def enq(
    elements: Seq[ValidIO[T]], ranges: Seq[(QueuePtr, UInt)], reserved: UInt
  ): Seq[Bool] = {
    require(elements.nonEmpty && elements.length <= numEnq)
    require(elements.length == ranges.length)
    val accepted = enqGuard(elements)
    val bounds = ranges.map { case (start, length) => (start, start + length) }
    for (slot <- 0 until entries) {
      val hits = bounds.zip(accepted).map { case ((start, end), accept) =>
        accept && Mux(start.flag =/= end.flag,
          start.value <= slot.U || slot.U < end.value,
          start.value <= slot.U && slot.U < end.value)
      }
      when(!flush && hits.reduce(_ || _)) { contents(slot) := PriorityMux(hits, elements.map(_.bits)) }
    }
    advanceEnq(reserved)
    accepted
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
