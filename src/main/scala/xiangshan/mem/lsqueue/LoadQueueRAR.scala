/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.mem

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import utils._
import utils.QueueModel.{FieldOf, IntentionCallback, Storage}
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._
import xiangshan.cache._

/** One physical RAR slot; ownership and release reset, keys and address do not. */
class LoadQueueRAREntry(implicit p: Parameters) extends XSBundle {
  val allocated = Bool()
  val released = Bool()
  val paddr = UInt(16.W)
  val robIdx = new RobPtr
  val lqIdx = new LqPtr
}

class LoadQueueRARFreeKey(implicit p: Parameters) extends XSBundle {
  val wb = new LqPtr
  val redirectRob = new RobPtr
  val redirectValid = Bool()
  val flushItself = Bool()
}

class LoadQueueRARQueryKey(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val paddr = UInt(16.W)
  val tso = Bool()
}

class LoadQueueRAR(implicit p: Parameters) extends XSModule
  with IntentionCallback
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(Valid(new Redirect))

    // violation query
    val query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO))

    // release cacheline
    val release = Flipped(Valid(new Release))

    // from VirtualLoadQueue
    val ldWbPtr = Input(new LqPtr)

    // global
    val lqFull = Output(Bool())

    // CSR controls
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })

  private val PartialPAddrStride: Int = 6
  private val PartialPAddrBits: Int = 16
  private val PartialPAddrLowBits: Int = (PartialPAddrBits - PartialPAddrStride) / 2 // avoid overlap
  private val PartialPAddrHighBits: Int = PartialPAddrBits - PartialPAddrLowBits
  private def boundary(x: Int, h: Int) = if (x < h) Some(x) else None
  private def lowMapping = (0 until PartialPAddrLowBits).map(i => Seq(
      boundary(PartialPAddrStride + i  , PartialPAddrBits),
      boundary(PartialPAddrBits - i - 1, PartialPAddrBits)
    )
  )
  private def highMapping = (0 until PartialPAddrHighBits).map(i => Seq(
      boundary(i + PartialPAddrStride     , PAddrBits),
      boundary(i + PartialPAddrStride + 11, PAddrBits),
      boundary(i + PartialPAddrStride + 22, PAddrBits),
      boundary(i + PartialPAddrStride + 33, PAddrBits)
    )
  )
  private def genPartialPAddr(paddr: UInt) = {
    val ppaddr_low = Wire(Vec(PartialPAddrLowBits, Bool()))
    ppaddr_low.zip(lowMapping).foreach {
      case (bit, mapping) =>
        bit := mapping.filter(_.isDefined).map(x => paddr(x.get)).reduce(_^_)
    }

    val ppaddr_high = Wire(Vec(PartialPAddrHighBits, Bool()))
    ppaddr_high.zip(highMapping).foreach {
      case (bit, mapping) =>
        bit := mapping.filter(_.isDefined).map(x => paddr(x.get)).reduce(_^_)
    }
    Cat(ppaddr_high.asUInt, ppaddr_low.asUInt)
  }

  println("LoadQueueRAR: size: " + LoadQueueRARSize)
  //  LoadQueueRAR field
  //  +-----------+-------------+-------+----------+
  //  | Allocated | ROB/LQ keys | PAddr | Released |
  //  +-----------+-------------+-------+----------+
  //
  //  Field descriptions:
  //  Allocated   : entry is valid.
  //  ROB/LQ keys : ordering and writeback predicates.
  //  PAddr       : physical address.
  //  Released    : DCache released.
  // Keep all per-entry predicates in Storage. Only operation results cross to RTL.
  private val entries: Storage[LoadQueueRAREntry] = Storage(
    new LoadQueueRAREntry, LoadQueueRARSize,
    Some(Seq.fill(LoadQueueRARSize)((new LoadQueueRAREntry).Lit(
      _.allocated -> false.B, _.released -> false.B))))
  private val allocatedField = FieldOf(new LoadQueueRAREntry)(_.allocated)
  private val releasedField = FieldOf(new LoadQueueRAREntry)(_.released)

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadQueueRARSize,
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = false,
    moduleName = "LoadQueueRAR freelist"
  ))
  freeList.io := DontCare

  // Real-allocation: load_s2
  // PAddr write needs 2 cycles, release signal should delay 1 cycle so that
  // load enqueue can catch release.
  val release1Cycle = io.release
  // val release2Cycle = RegNext(io.release)
  // val release2Cycle_dup_lsu = RegNext(io.release)
  val release2Cycle = RegEnable(io.release, io.release.valid)
  release2Cycle.valid := RegNext(io.release.valid)
  //val release2Cycle_dup_lsu = RegEnable(io.release, io.release.valid)

  // LoadQueueRAR enqueue condition:
  // There are still not completed load instructions before the current load instruction.
  // (e.g. "not completed" means that load instruction get the data or exception).
  val canEnqueue = io.query.map(_.req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val hasNotWritebackedLoad = io.query.map(_.req.bits.uop.lqIdx).map(lqIdx => isAfter(lqIdx, io.ldWbPtr))
  val needEnqueue = canEnqueue.zip(hasNotWritebackedLoad).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }

  // Allocate logic
  val acceptedVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueRARSize).W)))

  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    acceptedVec(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocate ready
    val offset = PopCount(needEnqueue.take(w))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex = Wire(UInt(log2Up(LoadQueueRARSize).W))
    enqIndex := freeList.io.allocateSlot(offset)
    enq.ready := Mux(needEnqueue(w), canAccept, true.B)

    enqIndexVec(w) := enqIndex
    when (needEnqueue(w) && enq.ready) {
      acceptedVec(w) := true.B

      freeList.io.doAllocate(w) := true.B
      //  Allocate new entry
      val entry = Wire(new LoadQueueRAREntry)
      entry.allocated := true.B
      entry.robIdx := enq.bits.uop.robIdx
      entry.lqIdx := enq.bits.uop.lqIdx
      entry.paddr := genPartialPAddr(enq.bits.paddr)

      //  NC is uncachable and will not be explicitly released.
      //  So NC requests are not allowed to have RAR
      entry.released := enq.bits.is_nc || (
        enq.bits.data_valid &&
        (release2Cycle.valid && enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset)
        || release1Cycle.valid && enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
      )
      entries.write(enqIndex, entry)
    }
    val debug_robIdx = enq.bits.uop.robIdx.asUInt
    XSError(
      needEnqueue(w) && enq.ready && entries.read(enqIndex).allocated,
      p"LoadQueueRAR: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")
  }

  //  LoadQueueRAR deallocate
  val freeMaskVec = Wire(Vec(LoadQueueRARSize, Bool()))

  // Reclaim and report exactly the same current-state matches to FreeList.
  val freeKey = Wire(new LoadQueueRARFreeKey)
  freeKey.wb := io.ldWbPtr
  freeKey.redirectRob := io.redirect.bits.robIdx
  freeKey.redirectValid := io.redirect.valid
  freeKey.flushItself := io.redirect.bits.flushItself()
  val canFree = (entry: LoadQueueRAREntry, key: LoadQueueRARFreeKey) => {
    val deqNotBlock = !isBefore(key.wb, entry.lqIdx)
    val needFlush = key.redirectValid &&
      ((key.flushItself && entry.robIdx === key.redirectRob) || isAfter(entry.robIdx, key.redirectRob))
    entry.allocated && (deqNotBlock || needFlush)
  }
  val reclaimMask = entries.camHitVec(canFree, freeKey)
  freeMaskVec := VecInit(reclaimMask)
  entries.writeMask(reclaimMask, allocatedField, false.B)

  // if need replay revoke entry
  val lastCanAccept = GatedRegNext(acceptedVec)
  val lastAllocIndex = GatedRegNext(enqIndexVec)

  for ((revoke, w) <- io.query.map(_.revoke).zipWithIndex) {
    val revokeValid = revoke && lastCanAccept(w)
    val revokeIndex = lastAllocIndex(w)

    val revokeAllocated = entries.read(revokeIndex).allocated
    when (revokeAllocated && revokeValid) {
      entries.writeField(revokeIndex, allocatedField, false.B)
      freeMaskVec(revokeIndex) := true.B
    }
  }

  freeList.io.free := freeMaskVec.asUInt

  // LoadQueueRAR Query
  // Load-to-Load violation check condition:
  // 1. Physical address match by CAM port, or we're in TSO mode
  // 2. release or nc_with_data is set.
  // 3. Younger than current load instruction.
  for ((query, w) <- io.query.zipWithIndex) {
    val key = Wire(new LoadQueueRARQueryKey)
    key.robIdx := query.req.bits.uop.robIdx
    key.paddr := genPartialPAddr(query.req.bits.paddr)
    key.tso := io.csrCtrl.tso_enable
    val violation = entries.camAny((entry: LoadQueueRAREntry, key: LoadQueueRARQueryKey) =>
      entry.allocated && entry.released &&
        (key.tso || entry.paddr === key.paddr) && isAfter(entry.robIdx, key.robIdx), key, query.req.valid)
    query.resp.valid := RegNext(query.req.valid)
    // OR commutes with this common register enable, including invalid-cycle hold.
    query.resp.bits.rep_frm_fetch := RegEnable(violation, query.req.valid)
  }

  // Retain the sampled physical slots even if a slot is freed or reused next cycle.
  // This write is last, preserving its priority over an enqueue's release value.
  val releaseMatches = entries.camHitVec((entry: LoadQueueRAREntry, hash: UInt) =>
    entry.allocated && entry.paddr === hash, genPartialPAddr(release1Cycle.bits.paddr), release1Cycle.valid)
  val releasePending = RegNext(VecInit(releaseMatches))
  entries.writeMask(releasePending.toSeq, releasedField, true.B)

  io.lqFull := freeList.io.empty

  // perf cnt
  val canEnqCount = PopCount(io.query.map(_.req.fire))
  val validCount = freeList.io.validCount
  val allowEnqueue = validCount <= (LoadQueueRARSize - LoadPipelineWidth).U
  val ldLdViolationCount = PopCount(io.query.map(_.resp).map(resp => resp.valid && resp.bits.rep_frm_fetch))

  QueuePerf(LoadQueueRARSize, validCount, !allowEnqueue)
  XSPerfAccumulate("enq", canEnqCount)
  XSPerfAccumulate("ld_ld_violation", ldLdViolationCount)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", canEnqCount),
    ("ld_ld_violation", ldLdViolationCount)
  )
  generatePerfEvent()
  // End
}
