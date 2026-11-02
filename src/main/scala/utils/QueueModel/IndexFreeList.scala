// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility._

class IndexFreeListIO(entries: Int, allocWidth: Int, indexW: Int, cntW: Int) extends Bundle {
  val allocateReq = Input(Vec(allocWidth, Bool()))
  val allocateSlot = Output(Vec(allocWidth, UInt(indexW.W)))
  val canAllocate = Output(Vec(allocWidth, Bool()))
  // Only the population count determines how many indices are consumed.
  val doAllocate = Input(Vec(allocWidth, Bool()))

  val free = Input(UInt(entries.W))

  // Indices not yet available for allocation, including pending returns.
  val validCount = Output(UInt(cntW.W))
  val empty = Output(Bool())
}

/** Allocate indices from a ring initialized full with `0 .. entries-1`.
  * Callers must avoid over-allocation and duplicate returns.
  */
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

  private val ring = Module(new IndexFreeListRing(entries, allocWidth, freeWidth, enablePreAlloc))
  ring.io.allocateReq := io.allocateReq
  ring.io.doAllocate := io.doAllocate
  io.allocateSlot := ring.io.allocateSlot
  io.canAllocate := ring.io.canAllocate
  // Register status from the same next pointers that commit in the ring.
  private val freeSlotCnt = RegInit(entries.U(cntW.W))
  freeSlotCnt := ring.io.nextFreeCount
  io.empty := freeSlotCnt === 0.U
  io.validCount := entries.U - freeSlotCnt

  // Stage 0: queue free requests; reuse takes at least three edges.
  private val s0FreeRequest = io.free
  private val s1PendingFree = RegInit(0.U(entries.W))
  private val s2EnqueuingMaskByPort = Wire(Vec(freeWidth, UInt(entries.W)))
  private val s2EnqueuingMask = s2EnqueuingMaskByPort.reduce(_ | _)

  // Stage 1: exclude returns already on their way to the ring.
  private val s1SelectableFree = s1PendingFree & ~s2EnqueuingMask

  private def residueBits(input: UInt, residue: Int): UInt =
    VecInit((0 until entries / freeWidth).map(i => input(freeWidth * i + residue))).asUInt

  // Each port selects from indices with index % freeWidth == port.
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

  // Stage 2: hold selected returns for the ring's next commit.
  private val s2EnqueueValid = GatedRegNext(VecInit(s1CandidatesByPort.map(_.asUInt.orR)))
  private val s2EnqueueIndexOH = GatedRegNext(s1SelectedIndexOH)

  s1PendingFree := (s0FreeRequest | s1PendingFree) & ~s2EnqueuingMask

  private val s2EnqueueIndex = s2EnqueueIndexOH.map(OHToUInt(_)).toSeq
  for (i <- 0 until freeWidth) {
    s2EnqueuingMaskByPort(i) := Mux(s2EnqueueValid(i), s2EnqueueIndexOH(i), 0.U(entries.W))
  }
  ring.io.enqueueValid := s2EnqueueValid
  ring.io.enqueueIndex := VecInit(s2EnqueueIndex)

  XSPerfAccumulate("empty", io.empty)
  val perfEvents: Seq[(String, UInt)] = Seq(("empty", io.empty))
  generatePerfEvent()
}


/** FIFO storage for free indices, initialized full. */
private[QueueModel] class IndexFreeListRing(
  entries: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean
)(implicit p: Parameters, expandRTL: Field[Boolean])
    extends RingQueue[UInt](UInt(log2Up(entries).W), entries, freeWidth, allocWidth) {
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
  when(io.doAllocate.asUInt.orR) { deq(PopCount(io.doAllocate)) }
  // Use the explicit mask so selected returns are never dropped by admission.
  enq(io.enqueueIndex.toSeq, io.enqueueValid.toSeq)
  emitRTL()

  for (i <- 0 until allocWidth) {
    // Preallocation previews from the head after this cycle's allocations.
    val base = if (enablePreAlloc) headPtrNext else deqPtr
    // Keep pointer steps separate to preserve non-power-of-two wrap behavior.
    val candidate = base + PopCount(io.allocateReq.take(i))
    val available = isBefore(candidate, enqPtr)
    val payload = read(candidate.value)
    if (enablePreAlloc) {
      // Register next-cycle candidates; outputs have no reset value.
      io.canAllocate(i) := RegEnable(available, true.B)
      io.allocateSlot(i) := RegEnable(payload, true.B)
    } else {
      io.canAllocate(i) := available
      io.allocateSlot(i) := payload
    }
  }
  io.nextFreeCount := distanceBetween(tailPtrNext, headPtrNext)
}
