// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility.HasCircularQueuePtrHelper

class SparseLaneRingQueueIO[T <: Data](gen: T, entries: Int, lanes: Int) extends Bundle {
  val enq = Vec(lanes, Flipped(Valid(gen)))
  val deq = DecoupledIO(gen)
  val canEnqueue = Output(Bool())
  val validCount = Output(UInt(log2Ceil(entries + 1).W))
}

/** Fixed-lane ring with observable holes, not a dense FIFO.
  *
  * Lane i writes the physical slot at tail+i. The tail advances by the number
  * of accepted inputs, so a missing lower lane leaves a hole instead of
  * packing a higher lane down. A hole at the head stalls dequeue. Every lane
  * writes its own slot even when an older entry already occupies it.
  *
  * Admission uses old pointer distance plus the offered count, with no credit
  * from a current-cycle dequeue. `validCount` counts slot validity and is
  * independent of pointer distance. Reads return old registered state and raw
  * payload bits, including when the head is invalid. Dequeue clears validity
  * at the edge; a same-slot enqueue write wins that clear.
  *
  * Reset installs each slot's original integer pattern via `i.U.asTypeOf`,
  * preserving payload, validity and padding bits. Emission happens at the end
  * of construction so update closures run in this module's context.
  */
class SparseLaneRingQueue[T <: Data](gen: T, entries: Int, lanes: Int)(
  implicit p: Parameters, expandRTL: Field[Boolean] = QueueRTL
) extends RingQueue[ValidIO[T]](Valid(gen), entries, lanes, 1)
    with HasCircularQueuePtrHelper {
  require(lanes > 0 && lanes <= entries)

  val io = IO(new SparseLaneRingQueueIO(gen, entries, lanes))

  /** Evaluate during superclass construction; only use constructor parameters. */
  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some((0 until entries).map(_.U.asTypeOf(Valid(gen))))

  private val offered = PopCount(io.enq.map(_.valid))
  io.canEnqueue := (distanceBetween(enqPtr, deqPtr) + offered) <= entries.U
  private val accepted = io.enq.map(element => element.valid && io.canEnqueue)

  private val head = deq(io.deq.fire.asUInt, Seq(0.U)).head
  io.deq.valid := head.valid
  io.deq.bits := head.bits

  private val slotValids = Wire(Vec(entries, Bool()))
  for (slot <- 0 until entries) {
    slotValids(slot) := read(slot).valid
  }
  io.validCount := PopCount(slotValids)

  // The dequeue clear is registered before the lane writes, so a same-slot
  // enqueue replaces the cleared entry and keeps its complete Valid payload.
  writeField(deqPtr.value, (entry: ValidIO[T]) => entry.valid, false.B, io.deq.fire)
  for (lane <- 0 until lanes) {
    write((enqPtr + lane.U).value, io.enq(lane), accepted(lane))
  }

  // A packed enqueue places accepted lanes at successive offsets from the tail,
  // but lane i must write tail+i even when a lower lane is absent. Register an
  // all-false definition only to satisfy the required enqueue before emission;
  // it writes no payload and advances the tail by zero.
  enq(io.enq.toSeq, Seq.fill(lanes)(false.B))

  // Exceptional direct advance: the fixed-lane writes above cannot be expressed
  // by the packed enqueue, and the tail moves by the total accepted count.
  advanceEnq(PopCount(accepted))

  // Emit inside this module so the registered update closures run in their
  // owner's elaboration context rather than the parent's.
  emitRTL()
}
