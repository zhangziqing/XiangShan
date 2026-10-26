// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import utils.QueueModel.{IndexedSegmentRing, QueueRTL}
import xiangshan._
import xiangshan.backend.Bundles.DynInst

/** One retained segment transaction: the legacy `data` / `uopq` / `stride`
  * register triple packed as one ring payload.
  */
class SegmentRingEntry(implicit p: Parameters) extends VLSUBundle {
  val uop    = new DynInst
  val data   = UInt(VLEN.W)
  val stride = UInt(VLEN.W)
}

/** Wrap-carrying circular pointer for [[SegmentRing]]; the `flag` bit is what
  * distinguishes a full ring from an empty one.
  */
class SegmentRingPtr(maxSize: Int) extends CircularQueuePtr[SegmentRingPtr](maxSize)

class SegmentRingReadPort(maxSize: Int)(implicit p: Parameters) extends VLSUBundle {
  val addr = Input(UInt(log2Ceil(maxSize).W))
  val data = Output(new SegmentRingEntry)
}

class SegmentRingMergePort(maxSize: Int)(implicit p: Parameters) extends VLSUBundle {
  val addr = UInt(log2Ceil(maxSize).W)
  val data = UInt(VLEN.W)
}

class SegmentRingIO(maxSize: Int, numRead: Int)(implicit p: Parameters) extends VLSUBundle {
  /** Enqueue: full transaction at the enqueue cursor, never back-pressured
    * (the caller keeps occupancy legal; the parent `io.in.ready` is always true).
    */
  val enq    = Flipped(Valid(new SegmentRingEntry))
  /** Dequeue: oldest transaction at the dequeue cursor. */
  val deq    = Decoupled(new SegmentRingEntry)
  /** Bounded data-only merge at an explicit physical slot. Applied after the
    * enqueue, so on a same-slot collision the incoming `uop`/`stride` survive
    * and only `data` is replaced by the merge.
    */
  val merge  = Flipped(Valid(new SegmentRingMergePort(maxSize)))
  /** Combinational point access for the split/stride cursors. */
  val read   = Vec(numRead, new SegmentRingReadPort(maxSize))
  /** Full wrap-carrying cursors, so split/stride arithmetic keeps the baseline
    * `CircularQueuePtr` flag semantics.
    */
  val enqPtr = Output(new SegmentRingPtr(maxSize))
  val deqPtr = Output(new SegmentRingPtr(maxSize))
  /** `isEmpty(enqPtr, deqPtr)`; the baseline has no other occupancy observable.
    */
  val empty  = Output(Bool())
}

/** Dedicated segment ring composed on [[IndexedSegmentRing]].
  *
  * The QueueModel core owns the two wrap-carrying cursors, the unreset payload
  * storage and the enqueue/dequeue transfer. This owner supplies the segment IO
  * and the indexed payload extension:
  *   - enqueue writes the whole transaction at `enqPtr.value`;
  *   - a merge at the same physical slot is folded into the enqueue payload's
  *     `data` field, so the core's enqueue-last whole-entry write keeps the
  *     legacy "enqueue uop/stride, merge data" priority;
  *   - a merge at any other physical slot is a field-only pre-enqueue `data`
  *     write, leaving `uop`/`stride` untouched;
  *   - dequeue presents the head entry and advances only on `fire`;
  *   - reads are raw combinational views of an explicit physical slot with no
  *     validity or reset gating;
  *   - payload registers stay unreset, so reset-concurrent enqueue/merge writes
  *     are captured exactly like the baseline `Reg` arrays.
  */
class SegmentRing(maxSize: Int, numRead: Int)(implicit p: Parameters)
  extends IndexedSegmentRing[SegmentRingEntry](
    new SegmentRingEntry, maxSize, 1, 1
  )(p, QueueRTL) {

  val io = IO(new SegmentRingIO(maxSize, numRead))

  io.enqPtr := enqPtr.asTypeOf(new SegmentRingPtr(maxSize))
  io.deqPtr := deqPtr.asTypeOf(new SegmentRingPtr(maxSize))
  io.empty  := empty

  // Legacy order is whole-entry enqueue, then a data-only merge on a coincident
  // slot. RingQueue emits the enqueue batch last, so route a same-slot merge
  // into the enqueue payload instead of writing `data` after that batch.
  val sameSlotMerge  = io.merge.valid && io.merge.bits.addr === enqPtr.value
  val enqCarriesMerge = sameSlotMerge && io.enq.valid

  // Field-only writes for slots not claimed by a same-cycle enqueue. These are
  // emitted before the deferred enqueue batch and are not reset-gated.
  indexedUpdate(io.merge.bits.addr, io.merge.valid && !enqCarriesMerge) { entry =>
    entry.data := io.merge.bits.data
  }

  // A coincident merge replaces only `data`; enqueue still owns uop/stride.
  val enqEntry = WireInit(io.enq.bits)
  when(enqCarriesMerge) {
    enqEntry.data := io.merge.bits.data
  }
  enq(Seq(enqEntry), io.enq.valid)
  deq(io.deq.fire)

  io.deq.valid := !empty
  io.deq.bits  := deqData(0)

  // Raw physical-slot views for the caller's split/stride cursors; no validity
  // or reset gating.
  for (i <- 0 until numRead) {
    io.read(i).data := indexedRead(io.read(i).addr)
  }

  emitRTL()
}
