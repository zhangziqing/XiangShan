// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

/** Owner-local IO for the FDP counter-filter ring.
  *
  * `enqNeed` is the raw per-load offer. The queue derives the legacy
  * `enqPtr + needRank >= deqPtr` admission from its own old-state pointers and
  * reports it on `enqAccepted`. `retireCount` is the registered
  * `last3CycleAlloc`, driving both the same-cycle validity clear and the
  * unconditional head advance.
  */
class CounterFilterQueueIO[T <: Data](
  gen: T,
  entries: Int,
  numEnq: Int,
  numDeq: Int,
  retireWidth: Int
) extends Bundle {
  val enqNeed = Vec(numEnq, Input(Bool()))
  val enqBits = Vec(numEnq, Input(gen))
  val enqAccepted = Vec(numEnq, Output(Bool()))
  val retireCount = Input(UInt(retireWidth.W))
  /** Pre-edge contents, including stale payload of invalid slots. */
  val peek = Output(Vec(entries, Valid(gen)))
}

/** FDP CounterFilter storage on the current RingQueue API.
  *
  * The legacy ring has three distinct non-quiescent behaviours that must be
  * preserved together: admission compares circular pointers without
  * same-cycle retirement credit, retirement clears the old head prefix and
  * advances the head by the registered `last3CycleAlloc` even when it exceeds
  * occupancy, and a same-slot enqueue writes payload while retirement clears
  * validity.
  *
  * RingQueue emits automatic enqueue writes after registered updates, so the
  * same-slot case is encoded by masking the incoming `valid` with the
  * retirement address: the payload still writes, and validity is already
  * false. Indexed retirement writes then clear every non-aliased retiring
  * slot.
  */
class CounterFilterQueue[T <: Data](
  gen: T,
  entries: Int,
  numEnq: Int,
  numDeq: Int,
  retireWidth: Int
)(implicit p: Parameters, expandRTL: Field[Boolean] = QueueRTL)
    extends RingQueue[ValidIO[T]](Valid(gen), entries, numEnq, numDeq) {

  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some(Seq.fill(entries)(0.U.asTypeOf(Valid(gen))))

  val io = IO(new CounterFilterQueueIO(gen, entries, numEnq, numDeq, retireWidth))

  // Old-state visibility of every physical slot for the CAM query.
  for (i <- 0 until entries) { io.peek(i) := read(i) }

  private val accepted = Wire(Vec(numEnq, Bool()))
  private val incoming = Wire(Vec(numEnq, Valid(gen)))
  for (i <- 0 until numEnq) {
    val needRank = PopCount(io.enqNeed.take(i))
    accepted(i) := io.enqNeed(i) && ((enqPtr + needRank) >= deqPtr)
    io.enqAccepted(i) := accepted(i)

    // For baseline-assert-legal traffic every needed lane is accepted (the
    // owner keeps the legacy assertion), so the accepted lanes are exactly the
    // needed lanes. RingQueue's compacted accepted-rank therefore equals the
    // legacy need-rank destination for every accepted lane, even when a
    // non-quiescent reset has left deqPtr ahead of enqPtr. Mask validity when
    // the destination is being retired.
    val acceptedRank = PopCount(accepted.take(i))
    val destination = enqPtr + acceptedRank
    val retireHit = (0 until numDeq).map { j =>
      (j.U < io.retireCount) && ((deqPtr + j.U).value === destination.value)
    }.reduce(_ || _)
    incoming(i).valid := !retireHit
    incoming(i).bits := io.enqBits(i)
  }
  enq(incoming.toSeq, accepted)

  // Retire the old head prefix regardless of occupancy. Normal deq advances by
  // the full registered count without an occupancy check; its candidate reads
  // are unused.
  for (j <- 0 until numDeq) {
    writeField((deqPtr + j.U).value, (entry: ValidIO[T]) => entry.valid, false.B, j.U < io.retireCount)
  }
  deq(io.retireCount)
}
