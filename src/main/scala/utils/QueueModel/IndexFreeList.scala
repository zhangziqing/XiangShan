// SPDX-License-Identifier: MulanPSL-2.0
// Free-index allocator: ordered allocation from a reset-full identity ring,
// with return bits accumulated for a registered selection stage.
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility._

/** Ordered free-index allocator. The Queue payload is a slot index, not the
  * object stored by the caller. Reset represents a full ring whose contents are
  * the identity sequence `0 .. size-1`; both pointers reset so that the ring is
  * full. This is deliberately not an ordinary reset-empty FIFO.
  *
  * Return path (baseline timing preserved):
  *   - `free` accumulates a pending bitmap.
  *   - A residue-class selection picks at most one pending index per
  *     `freeWidth` residue class, excluding indices already selected.
  *   - Selection masks and one-hot indices are registered, then enter the ring
  *     on the following cycle. A same-cycle `free` is not an immediate
  *     allocation candidate.
  *   - Pending update is `(free | freeMask) & ~freeSelMask`; re-freeing a pending
  *     index is not reference counted and is a caller contract violation.
  *
  * Allocation path:
  *   - `canAllocate(i)` and `allocateSlot(i)` describe the candidate at
  *     `head + PopCount(allocateReq.take(i))`.
  *   - Only the population count of `doAllocate` is significant, as in the
  *     baseline FreeList. Lane positions do not identify allocated entries.
  *   - Without preallocation, candidates are combinational from current state.
  *   - With `enablePreAlloc`, candidates are registered (`RegEnable` enabled by
  *     the flag) from current `allocateReq`, current head plus current
  *     `doAllocate` count, and current tail/payload. `RegEnable` has no reset
  *     value: outputs before the first enabled edge are unspecified and must
  *     not be consumed. `doAllocate(t+1)` consumes that registered snapshot,
  *     which corresponds to the request pattern at t.
  *
  * Pointer transitions retain the baseline behavior even for illegal requests:
  * every registered return advances the tail in port order, and every asserted
  * `doAllocate` advances the head by the population count. Callers still own
  * ownership and capacity discipline, but violations retain the legacy state
  * transition rather than being silently truncated by this abstraction.
  * `validCount` counts indices outside the available ring, including returns
  * still in the pending/registered pipeline; it is not the number of free
  * indices.
  */
class IndexFreeListIO(size: Int, allocWidth: Int, indexW: Int, cntW: Int) extends Bundle {
  val allocateReq = Input(Vec(allocWidth, Bool()))
  val allocateSlot = Output(Vec(allocWidth, UInt(indexW.W)))
  val canAllocate = Output(Vec(allocWidth, Bool()))
  val doAllocate = Input(Vec(allocWidth, Bool()))

  val free = Input(UInt(size.W))

  val validCount = Output(UInt(cntW.W))
  val empty = Output(Bool())
}

class IndexFreeList(
  size: Int,
  allocWidth: Int,
  freeWidth: Int,
  enablePreAlloc: Boolean = false,
  moduleName: String = ""
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends Module with HasPerfEvents {
  require(size >= 2, "IndexFreeList needs at least two indices")
  require(allocWidth > 0, "allocation width must be positive")
  require(freeWidth > 0, "return width must be positive")
  require(size % freeWidth == 0, "size must be divisible by freeWidth")

  val indexW = log2Up(size)
  val cntW = log2Ceil(size + 1)

  val io = IO(new IndexFreeListIO(size, allocWidth, indexW, cntW))

  if (moduleName.nonEmpty) {
    println(s"IndexFreeList: $moduleName, size $size")
  }

  // Only static event/preview IO crosses the child Module boundary.
  private val ring = Module(new IndexFreeListRing(size, allocWidth, freeWidth, enablePreAlloc))
  ring.io.allocateReq := io.allocateReq
  ring.io.doAllocate := io.doAllocate
  io.allocateSlot := ring.io.allocateSlot
  io.canAllocate := ring.io.canAllocate
  // Preserve the original registered observation and its reset/update cone.
  // QueueStatus itself remains a combinational observation of child pointers.
  private val freeSlotCnt = RegInit(size.U(cntW.W))
  freeSlotCnt := ring.io.nextFreeCount
  io.empty := freeSlotCnt === 0.U
  io.validCount := size.U - freeSlotCnt

  // Return-side pending bitmap and registered selection.
  private val freeMask = RegInit(0.U(size.W))
  private val freeSelMaskVec = Wire(Vec(freeWidth, UInt(size.W)))
  private val freeSelMask = freeSelMaskVec.reduce(_ | _)
  private val pending = freeMask & ~freeSelMask

  private def remBits(input: UInt, rem: Int): UInt =
    VecInit((0 until size / freeWidth).map(i => input(freeWidth * i + rem))).asUInt

  private val remFreeSelMaskVec = VecInit(Seq.tabulate(freeWidth) { rem =>
    remBits(pending, rem)
  })
  private val remFreeSelIndexOHVec = VecInit(Seq.tabulate(freeWidth) { fport =>
    val highIndexOH = PriorityEncoderOH(remFreeSelMaskVec(fport))
    val freeIndexOHVec = Wire(Vec(size, Bool()))
    freeIndexOHVec.foreach(_ := false.B)
    for (i <- 0 until size / freeWidth) {
      freeIndexOHVec(i * freeWidth + fport) := highIndexOH(i)
    }
    freeIndexOHVec.asUInt
  })

  private val freeReq = GatedRegNext(VecInit(remFreeSelMaskVec.map(_.asUInt.orR)))
  private val freeSlotOH = GatedRegNext(remFreeSelIndexOHVec)

  freeMask := (io.free | freeMask) & ~freeSelMask

  private val returnedSlots = freeSlotOH.map(OHToUInt(_)).toSeq
  for (i <- 0 until freeWidth) {
    freeSelMaskVec(i) := Mux(freeReq(i), freeSlotOH(i), 0.U(size.W))
  }
  ring.io.freeReq := freeReq
  ring.io.freeSlot := VecInit(returnedSlots)

  // Retain the baseline debug counter and two-cycle hardware event.
  XSPerfAccumulate("empty", io.empty)
  val perfEvents: Seq[(String, UInt)] = Seq(("empty", io.empty))
  generatePerfEvent()
}


/** Queue-owned reset-full storage and endpoints. The allocator wrapper uses
  * preview and return events; it never reads child state across the Module IO.
  * Keep legacy sparse-port write priority and chained pointer additions.
  */
private[QueueModel] class IndexFreeListRing(
  size: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean
)(implicit p: Parameters, expandRTL: Field[Boolean])
    extends ElementQueue[UInt](UInt(log2Up(size).W), size) with QueueStatus {
  override protected def enqPtrInit: QueuePtr = {
    val ptr = Wire(new QueuePtr)
    ptr.value := 0.U
    ptr.flag := true.B
    ptr
  }
  override protected def initContents: Option[Seq[UInt]] =
    Some(Seq.tabulate(size)(i => i.U(log2Up(size).W)))

  val io = IO(new Bundle {
    val allocateReq = Input(Vec(allocWidth, Bool()))
    val doAllocate = Input(Vec(allocWidth, Bool()))
    val allocateSlot = Output(Vec(allocWidth, UInt(log2Up(size).W)))
    val canAllocate = Output(Vec(allocWidth, Bool()))
    val freeReq = Input(Vec(freeWidth, Bool()))
    val freeSlot = Input(Vec(freeWidth, UInt(log2Up(size).W)))
    val nextFreeCount = Output(UInt(log2Ceil(size + 1).W))
  })

  private val headPtrNext = deqPtr + PopCount(io.doAllocate)
  private val tailPtrNext = enqPtr + PopCount(io.freeReq)
  when(io.doAllocate.asUInt.orR) { deqPtrReg := headPtrNext }
  when(io.freeReq.asUInt.orR) { enqPtrReg := tailPtrNext }

  // Retain the original per-port destination/rank and last-write priority.
  for (i <- 0 until freeWidth) {
    val destination = (enqPtr + PopCount(io.freeReq.take(i))).value
    update { entry =>
      when(io.freeReq(i) && destination === physicalSlot(entry).U) {
        entry := io.freeSlot(i)
      }
    }
  }
  emitElementUpdates()

  // Vec indexing preserves the reference read mux, including invalid lanes.
  private val elements = scala.collection.mutable.ArrayBuffer.empty[UInt]
  foreach { entry => elements += entry }
  private val readView = VecInit(elements.toSeq)
  for (i <- 0 until allocWidth) {
    val base = if (enablePreAlloc) headPtrNext else deqPtr
    val candidate = base + PopCount(io.allocateReq.take(i))
    val available = isBefore(candidate, enqPtr)
    val payload = readView(candidate.value)
    if (enablePreAlloc) {
      io.canAllocate(i) := RegEnable(available, true.B)
      io.allocateSlot(i) := RegEnable(payload, true.B)
    } else {
      io.canAllocate(i) := available
      io.allocateSlot(i) := payload
    }
  }
  io.nextFreeCount := distanceBetween(tailPtrNext, headPtrNext)
}
