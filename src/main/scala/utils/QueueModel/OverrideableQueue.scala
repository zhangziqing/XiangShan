// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.Data._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

class OverrideableQueueIO[T <: Data](gen: T) extends Bundle {
  val in = Flipped(Valid(gen))
  val out = Decoupled(gen)
}

/** Independent read/write positions with per-slot validity, not dense occupancy.
  * Every Valid input advances the writer, replacing any occupied slot. Output
  * fire advances the reader. Reads observe old state; same-slot enqueue wins
  * validity over dequeue and element updates, with no input bypass.
  *
  * Preserve the original unsigned log2Up(entries)-bit binary positions: unused
  * positions drop writes and read zero, except that the singleton Mux1H always
  * exposes its sole slot. Reset clears positions and validity, not payload.
  * Call emitRTL after complete object construction to apply registered updates.
  */
class OverrideableQueue[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters, expandRTL: Field[Boolean]
) extends QueueBase[ValidIO[T]](entries) {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")

  override protected def pointerIndexWidth: Int = log2Up(entries)

  protected def initContents: Seq[ValidIO[T]] =
    Seq.fill(entries)(Valid(gen).Lit(_.valid -> false.B))

  private val contents = {
    val initial = initContents
    require(initial.length == entries, "initial contents must describe exactly one entry per slot")
    RegInit(VecInit(initial))
  }
  private var modifyElement: ValidIO[T] => Unit = _ => ()
  private var enqCallback: () => Unit = () => ()
  private var emitted = false

  protected final def read(index: Int): ValidIO[T] = contents(index).readOnly

  /** Keep the original one-hot read circuit, including singleton behavior. */
  protected final def read(index: UInt): ValidIO[T] =
    Mux1H((0 until entries).map(slot => index === slot.U), contents).readOnly

  override protected final def foreach(observe: ValidIO[T] => Unit): Unit =
    contents.foreach(entry => observe(entry.readOnly))

  protected final def compose(
    first: ValidIO[T] => Unit, second: ValidIO[T] => Unit
  ): ValidIO[T] => Unit = entry => { first(entry); second(entry) }

  /** Put Chisel conditions inside the callback; emission runs it later. */
  override protected final def update(modify: ValidIO[T] => Unit): Unit = {
    require(!emitted, "element updates must be registered before emission")
    modifyElement = compose(modifyElement, modify)
  }

  override protected final def exists(predicate: ValidIO[T] => Bool): Bool = {
    val result = WireDefault(false.B)
    foreach { entry => when(predicate(entry)) { result := true.B } }
    result
  }

  private def physicalSlot(entry: ValidIO[T]): Int = {
    val slot = contents.indexWhere(_ eq entry)
    require(slot >= 0, "slot identity requires this Queue's callback entry")
    slot
  }

  /** Register the final Valid input; admission is independent of the reader. */
  protected def enq(element: ValidIO[T]): Bool = {
    require(!emitted, "enqueue must be registered before emission")
    enqCallback = () => {
      when(element.valid) { enqPtr.value := enqPtr.value + 1.U }
      update { entry =>
        when(element.valid && enqPtr.value === physicalSlot(entry).U) {
          entry.valid := true.B
          entry.bits := element.bits
        }
      }
    }
    element.valid
  }

  /** Read the current slot and consume it when valid and ready. */
  protected def deq(ready: Bool): ValidIO[T] = {
    val head = read(deqPtr.value)
    val consume = head.valid && ready
    when(consume) { deqPtr.value := deqPtr.value + 1.U }
    update { entry =>
      when(consume && deqPtr.value === physicalSlot(entry).U) { entry.valid := false.B }
    }
    head
  }

  val io = IO(new OverrideableQueueIO(gen))
  private val head = deq(io.out.ready)
  io.out.valid := head.valid
  io.out.bits := head.bits
  enq(io.in)

  /** Emit once after subclasses register updates; enqueue has last priority. */
  final def emitRTL(): this.type = {
    require(!emitted, "element updates must be emitted exactly once")
    enqCallback()
    emitted = true
    contents.foreach(modifyElement)
    this
  }
}
