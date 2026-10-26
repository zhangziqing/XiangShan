// SPDX-License-Identifier: MulanPSL-2.0
// Independent reader/writer staging policy over queue-owned element storage.
package utils.QueueModel

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config.{Field, Parameters}

class OverrideableQueueIO[T <: Data](gen: T) extends Bundle {
  val in = Flipped(Valid(gen))
  val out = Decoupled(gen)
}

/** A Valid input writes at the write position and advances it unconditionally.
  * Output fire clears the read slot and advances the independent read position.
  * A write to an occupied slot replaces its payload without moving the reader.
  * A write wins slot validity on a same-slot read/write; output observes the
  * current payload, with no same-cycle input bypass.
  *
  * Positions use unsigned log2Up(entries) bits and wrap by truncation, as in
  * the baseline. For a non-power-of-two capacity above one, unused positions
  * drop writes and present an invalid zero output. The baseline's singleton
  * Mux1H exposes its sole entry regardless of the read index; retain that too.
  * Reset clears slot validity and positions; payload has no reset guarantee.
  * This component has no dense-window/count contract. Entry extensions use
  * the inherited foreach/update callbacks.
  *
  * The clear and write connections intentionally touch the same valid bit in
  * that order, so the write has last-connect priority. Output data comes from
  * the register, not from the input, so a same-slot write is visible only after
  * the clock edge.
  */
class OverrideableQueue[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters, expandRTL: Field[Boolean]
) extends ElementQueue[ValidIO[T]](Valid(gen), entries) {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")

  // The legacy single-slot queue also retains a one-bit binary index.
  override protected def pointerIndexWidth: Int = log2Up(entries)

  val io = IO(new OverrideableQueueIO(gen))

  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some(Seq.fill(entries)(Valid(gen).Lit(_.valid -> false.B)))

  // Only physical pointer indices are observed. Independent wraps need no
  // dense ordering invariant; QueueBase's phase bits are otherwise unobserved.
  when(io.in.valid) { enqPtrReg.value := enqPtr.value + 1.U }
  when(io.out.fire) { deqPtrReg.value := deqPtr.value + 1.U }

  io.out.valid := false.B
  io.out.bits := 0.U.asTypeOf(gen)
  foreach { entry =>
    if (entries == 1) {
      io.out.valid := entry.valid
      io.out.bits := entry.bits
    } else {
      when(deqPtr.value === physicalSlot(entry).U) {
        io.out.valid := entry.valid
        io.out.bits := entry.bits
      }
    }
  }

  // The same-slot write wins validity, while the output reads old payload.
  update { entry =>
    val slot = physicalSlot(entry)
    when(io.out.fire && deqPtr.value === slot.U) { entry.valid := false.B }
    when(io.in.valid && enqPtr.value === slot.U) {
      entry.valid := true.B
      entry.bits := io.in.bits
    }
  }
  emitElementUpdates()
}
