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

  private val head = read(deqPtr.value)
  io.deq.valid := head.valid
  io.deq.bits := head.bits
  advanceDeq(io.deq.fire.asUInt)

  private val slotValids = Wire(Vec(entries, Bool()))
  for (slot <- 0 until entries) {
    slotValids(slot) := read(slot).valid
  }
  io.validCount := PopCount(slotValids)

  // Dequeue clear is registered before the enqueue writes, so a same-slot
  // enqueue replaces the cleared entry and keeps its complete Valid payload.
  update { entry =>
    when(io.deq.fire && deqPtr.value === physicalSlot(entry).U) {
      entry.valid := false.B
    }
  }
  update { entry =>
    val slot = physicalSlot(entry).U
    for (lane <- 0 until lanes) {
      when(accepted(lane) && (enqPtr + lane.U).value === slot) {
        entry := io.enq(lane)
      }
    }
  }

  when(!recover && !flush && VecInit(accepted).asUInt.orR) {
    enqPtr := enqPtr + PopCount(accepted)
  }

  // Emit inside this module so the registered update closures run in their
  // owner's elaboration context rather than the parent's.
  emitRTL()
}
