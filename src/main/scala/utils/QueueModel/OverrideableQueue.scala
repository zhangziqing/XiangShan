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
  * Use QueuePtr's capacity wrap and phase, including a zero-bit singleton index.
  * Both positions always address physical slots. Per-slot validity determines
  * membership; pointer distance is not occupancy after unrestricted overwrite.
  * Reset clears positions and validity, not payload.
  * RingQueue owns storage, update composition and final enqueue emission. Call
  * emitRTL after complete object construction to apply registered updates.
  */
class OverrideableQueue[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters, expandRTL: Field[Boolean]
) extends RingQueue[ValidIO[T]](Valid(gen), entries, numEnq = 1, numDeq = 1) {

  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some(Seq.fill(entries)(Valid(gen).Lit(_.valid -> false.B)))

  /** Select the current physical head using the original one-hot read circuit. */
  private def readHead: ValidIO[T] =
    Mux1H((0 until entries).map(slot => deqPtr.value === slot.U), (0 until entries).map(read)).readOnly

  /** Register the final Valid input; admission is independent of the reader. */
  protected def enq(element: ValidIO[T]): Bool = {
    super.enq(Seq(element), Seq(element.valid))
    element.valid
  }

  /** Read the current slot and consume it when valid and ready. */
  protected def deq(ready: Bool): ValidIO[T] = {
    val head = readHead
    val consume = head.valid && ready
    advanceDeq(consume.asUInt)
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
}
