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
)(implicit p: Parameters, expandRTL: Field[Boolean]) extends RingQueue[UInt](
  UInt(log2Up(size).W), size, freeWidth, allocWidth
) with HasPerfEvents {
  require(size >= 2, "IndexFreeList needs at least two indices")
  require(allocWidth > 0, "allocation width must be positive")
  require(freeWidth > 0, "return width must be positive")
  require(size % freeWidth == 0, "size must be divisible by freeWidth")

  val indexW = log2Up(size)
  val cntW = log2Ceil(size + 1)
  val rankW = math.max(1, log2Ceil(allocWidth))

  val io = IO(new IndexFreeListIO(size, allocWidth, indexW, cntW))

  if (moduleName.nonEmpty) {
    println(s"IndexFreeList: $moduleName, size $size")
  }

  override protected def enqPtrInit: QueuePtr = {
    val ptr = Wire(new QueuePtr)
    ptr.value := 0.U
    ptr.flag := true.B
    ptr
  }
  override protected def initContents: Option[Seq[UInt]] =
    Some(Seq.tabulate(size)(i => i.U(log2Up(size).W)))

  /** PopCount can remain a Bool object for a one-bit input. */
  private def countOf(bits: Seq[Bool], minWidth: Int): UInt = {
    val raw = PopCount(bits)
    val normalized = Wire(UInt(math.max(raw.getWidth, minWidth).W))
    normalized := raw
    normalized
  }

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
  // Preserve direct-ring behavior for over-return inputs as well.
  enq(packEnqueue(returnedSlots, freeReq), countOf(freeReq, 1))

  // Allocation-side candidates. `reqRank` is the request-prefix rank used by
  // both the exposed candidate and the delayed doAllocate consumption.
  private val numAllocate = countOf(io.doAllocate, rankW)
  private val reqRank = (0 until allocWidth).map { i =>
    countOf(io.allocateReq.take(i), rankW)
  }
  private val offsetW = log2Ceil(size + allocWidth + 1)

  for (i <- 0 until allocWidth) {
    val offset = Wire(UInt(offsetW.W))
    if (enablePreAlloc) {
      val rankWide = Wire(UInt(offsetW.W))
      val allocWide = Wire(UInt(offsetW.W))
      rankWide := reqRank(i)
      allocWide := numAllocate
      offset := rankWide + allocWide
    } else {
      offset := reqRank(i)
    }
    // `isBefore` is the legacy circular-order predicate. It deliberately
    // differs from a distance bound when callers drive an invalid pointer
    // interval, and preserves the original behavior for those inputs too.
    val candidatePtr = deqPtr + offset
    val candidateValid = isBefore(candidatePtr, enqPtr)
    val candidateData = WireDefault(0.U(indexW.W))
    val index = candidatePtr.value
    foreach { entry => when(index === physicalSlot(entry).U) { candidateData := entry } }

    if (enablePreAlloc) {
      // RegEnable has no reset value. Its outputs are meaningful only after an
      // enabled edge, so reset must not be treated as a valid candidate.
      io.canAllocate(i) := RegEnable(candidateValid, enablePreAlloc.B)
      io.allocateSlot(i) := RegEnable(candidateData, enablePreAlloc.B)
    } else {
      io.canAllocate(i) := candidateValid
      io.allocateSlot(i) := candidateData
    }
  }

  // Preserve direct-ring behavior for sparse and over-allocation inputs.
  deq(numAllocate)

  io.empty := count === 0.U
  io.validCount := (size.U - count)

  // Retain the baseline debug counter and two-cycle hardware event.
  XSPerfAccumulate("empty", io.empty)
  val perfEvents: Seq[(String, UInt)] = Seq(("empty", io.empty))
  generatePerfEvent()
  emitRTL()
}
