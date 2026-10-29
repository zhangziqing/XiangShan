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
  * the identity sequence `0 .. entries-1`; both pointers reset so that the ring is
  * full. This is deliberately not an ordinary reset-empty FIFO.
  *
  * Return path (baseline timing preserved):
  *   - Stage 0 requests (`free`) accumulate in the pending bitmap.
  *   - Stage 1 selects at most one pending index per
  *     `freeWidth` residue class, excluding indices already selected.
  *   - Stage 2 registers the selection for enqueue; stage 3 commits it to the
  *     ring. A same-cycle `free` is not an immediate allocation candidate.
  *   - Pending update is `(s0FreeRequest | s1PendingFree) & ~s2EnqueuingMask`;
  *     re-freeing a pending index is not reference counted and is a caller
  *     contract violation.
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
class IndexFreeListIO(entries: Int, allocWidth: Int, indexW: Int, cntW: Int) extends Bundle {
  val allocateReq = Input(Vec(allocWidth, Bool()))
  val allocateSlot = Output(Vec(allocWidth, UInt(indexW.W)))
  val canAllocate = Output(Vec(allocWidth, Bool()))
  val doAllocate = Input(Vec(allocWidth, Bool()))

  val free = Input(UInt(entries.W))

  val validCount = Output(UInt(cntW.W))
  val empty = Output(Bool())
}

class IndexFreeList(
  entries: Int,
  allocWidth: Int,
  freeWidth: Int,
  enablePreAlloc: Boolean = false,
  moduleName: String = ""
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends Module with HasPerfEvents {
  require(entries >= 2, "IndexFreeList needs at least two indices")
  require(allocWidth > 0, "allocation width must be positive")
  require(freeWidth > 0, "return width must be positive")
  require(entries % freeWidth == 0, "entries must be divisible by freeWidth")

  val indexW = log2Up(entries)
  val cntW = log2Ceil(entries + 1)

  val io = IO(new IndexFreeListIO(entries, allocWidth, indexW, cntW))

  if (moduleName.nonEmpty) {
    println(s"IndexFreeList: $moduleName, entries $entries")
  }

  // Only static event/preview IO crosses the child Module boundary.
  private val ring = Module(new IndexFreeListRing(entries, allocWidth, freeWidth, enablePreAlloc))
  ring.io.allocateReq := io.allocateReq
  ring.io.doAllocate := io.doAllocate
  io.allocateSlot := ring.io.allocateSlot
  io.canAllocate := ring.io.canAllocate
  // Preserve the original registered observation and its reset/update cone.
  // QueueStatus itself remains a combinational observation of child pointers.
  private val freeSlotCnt = RegInit(entries.U(cntW.W))
  freeSlotCnt := ring.io.nextFreeCount
  io.empty := freeSlotCnt === 0.U
  io.validCount := entries.U - freeSlotCnt

  // Stage 0: requests enter the pending bitmap at the next edge.
  private val s0FreeRequest = io.free
  private val s1PendingFree = RegInit(0.U(entries.W))
  private val s2EnqueuingMaskByPort = Wire(Vec(freeWidth, UInt(entries.W)))
  private val s2EnqueuingMask = s2EnqueuingMaskByPort.reduce(_ | _)

  // Stage 1: select pending indices not already enqueuing in stage 2.
  private val s1SelectableFree = s1PendingFree & ~s2EnqueuingMask

  private def residueBits(input: UInt, residue: Int): UInt =
    VecInit((0 until entries / freeWidth).map(i => input(freeWidth * i + residue))).asUInt

  private val s1CandidatesByPort = VecInit(Seq.tabulate(freeWidth) { residue =>
    residueBits(s1SelectableFree, residue)
  })
  private val s1SelectedIndexOH = VecInit(Seq.tabulate(freeWidth) { port =>
    val selectedResidueOH = PriorityEncoderOH(s1CandidatesByPort(port))
    val selectedIndexOH = Wire(Vec(entries, Bool()))
    selectedIndexOH.foreach(_ := false.B)
    for (i <- 0 until entries / freeWidth) {
      selectedIndexOH(i * freeWidth + port) := selectedResidueOH(i)
    }
    selectedIndexOH.asUInt
  })

  // Stage 2: registered selection drives the ring's enqueue ports.
  private val s2EnqueueValid = GatedRegNext(VecInit(s1CandidatesByPort.map(_.asUInt.orR)))
  private val s2EnqueueIndexOH = GatedRegNext(s1SelectedIndexOH)

  s1PendingFree := (s0FreeRequest | s1PendingFree) & ~s2EnqueuingMask

  private val s2EnqueueIndex = s2EnqueueIndexOH.map(OHToUInt(_)).toSeq
  for (i <- 0 until freeWidth) {
    s2EnqueuingMaskByPort(i) := Mux(s2EnqueueValid(i), s2EnqueueIndexOH(i), 0.U(entries.W))
  }
  ring.io.enqueueValid := s2EnqueueValid
  ring.io.enqueueIndex := VecInit(s2EnqueueIndex)
  // Stage 3: the next edge commits these indices and the ring's tail together.

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
  entries: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean
)(implicit p: Parameters, expandRTL: Field[Boolean])
    extends ElementQueue[UInt](UInt(log2Up(entries).W), entries) with QueueStatus {
  override protected def enqPtrInit: QueuePtr = {
    val ptr = Wire(new QueuePtr)
    ptr.value := 0.U
    ptr.flag := true.B
    ptr
  }
  override protected def initContents: Option[Seq[UInt]] =
    Some(Seq.tabulate(entries)(i => i.U(log2Up(entries).W)))

  val io = IO(new Bundle {
    val allocateReq = Input(Vec(allocWidth, Bool()))
    val doAllocate = Input(Vec(allocWidth, Bool()))
    val allocateSlot = Output(Vec(allocWidth, UInt(log2Up(entries).W)))
    val canAllocate = Output(Vec(allocWidth, Bool()))
    val enqueueValid = Input(Vec(freeWidth, Bool()))
    val enqueueIndex = Input(Vec(freeWidth, UInt(log2Up(entries).W)))
    val nextFreeCount = Output(UInt(log2Ceil(entries + 1).W))
  })

  private val headPtrNext = deqPtr + PopCount(io.doAllocate)
  private val tailPtrNext = enqPtr + PopCount(io.enqueueValid)
  when(io.doAllocate.asUInt.orR) { deqPtr := headPtrNext }
  when(io.enqueueValid.asUInt.orR) { enqPtr := tailPtrNext }

  // Retain the original per-port destination/rank and last-write priority.
  for (i <- 0 until freeWidth) {
    val destination = (enqPtr + PopCount(io.enqueueValid.take(i))).value
    update { entry =>
      when(io.enqueueValid(i) && destination === physicalSlot(entry).U) {
        entry := io.enqueueIndex(i)
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
