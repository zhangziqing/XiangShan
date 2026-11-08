// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

/** Owner-local IO for the SMS train-filter ring.
  *
  * The parent supplies one packed request per load/store lane and the
  * permutation's `needAlloc` decision. The queue computes the legacy
  * `enqPtr + needRank >= deqPtr` admission from its own old-state pointers,
  * exposes every physical slot for old-state dedup, and drains at most one
  * valid head per cycle.
  */
class SmsTrainFilterQueueIO[T <: Data](gen: T, entries: Int, numEnq: Int) extends Bundle {
  val enqNeed = Vec(numEnq, Input(Bool()))
  val enqBits = Vec(numEnq, Input(gen))
  val enqAccepted = Vec(numEnq, Output(Bool()))
  /** Pre-edge contents, including stale payload of invalid slots. */
  val peek = Output(Vec(entries, Valid(gen)))
  /** Pre-edge head; `valid` false means no consumption this cycle. */
  val deq = Output(Valid(gen))
}

/** SMS TrainFilter storage on the current RingQueue API.
  *
  * This is deliberately owner-local rather than a shared peekable queue: the
  * SMS policy is the only consumer of its per-slot peek and legacy pointer
  * admission interface. Admission and dequeue use the legacy circular pointer
  * comparison, not a dense count, so the old-state semantics survive reset and
  * wrap. Payload reset stays zero through `initContents`, matching the
  * baseline raw invalid output.
  */
class SmsTrainFilterQueue[T <: Data](
  gen: T,
  entries: Int,
  numEnq: Int
)(implicit p: Parameters, expandRTL: Field[Boolean] = QueueRTL)
    extends RingQueue[ValidIO[T]](Valid(gen), entries, numEnq, numDeq = 1) {

  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some(Seq.fill(entries)(0.U.asTypeOf(Valid(gen))))

  val io = IO(new SmsTrainFilterQueueIO(gen, entries, numEnq))

  // Old-state visibility of every physical slot for block-tag dedup.
  for (i <- 0 until entries) { io.peek(i) := read(i) }

  private val accepted = Wire(Vec(numEnq, Bool()))
  private val incoming = Wire(Vec(numEnq, Valid(gen)))
  for (i <- 0 until numEnq) {
    val rank = PopCount(io.enqNeed.take(i))
    accepted(i) := io.enqNeed(i) && ((enqPtr + rank) >= deqPtr)
    io.enqAccepted(i) := accepted(i)
    incoming(i).valid := true.B
    incoming(i).bits := io.enqBits(i)
  }
  // RingQueue compacts accepted lanes in port order. In the dense SMS interval
  // an accepted needed lane implies every earlier needed lane is accepted, so
  // the compacted accepted-rank equals the baseline need-rank. Enqueue
  // destinations are free while the dequeue head is occupied (or invalid), so
  // an enqueue/dequeue alias is not reachable and has no priority case to
  // preserve here.
  enq(incoming.toSeq, accepted)

  // One valid head drains per cycle through the normal transfer. Invalid heads
  // leave the pointer and stale payload untouched, preserving the raw invalid
  // output.
  private val head = read(deqPtr.value)
  io.deq.valid := head.valid
  io.deq.bits := head.bits
  writeField(deqPtr.value, (entry: ValidIO[T]) => entry.valid, false.B, head.valid)
  deq(head.valid.asUInt)
}
