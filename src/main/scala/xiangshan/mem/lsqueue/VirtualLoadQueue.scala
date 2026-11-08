/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import utils._
import utils.QueueModel.{QueueRTL, RingQueue}
import utility._
import xiangshan._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, UopIdx}
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}
import xiangshan.backend.fu.FuType
import xiangshan.mem.Bundles._
import xiangshan.cache._

class VirtualLoadQueuePayload(implicit p: Parameters) extends XSBundle {
  val valid      = Bool()
  val robIdx     = new RobPtr
  val uopIdx     = UopIdx()
  val isvec      = Bool()
  val committed  = Bool()
  val debug_mmio = Bool()
  val debug_paddr = UInt(PAddrBits.W)
}

/** The public protocol stays on VirtualLoadQueue; payload, validity and both
  * circular cursors are the inherited RingQueue state. The owner keeps the
  * original lane-0 pointer policy, delayed retirement and redirect clamp.
  */
class VirtualLoadQueue(implicit p: Parameters)
  extends RingQueue[VirtualLoadQueuePayload](
    new VirtualLoadQueuePayload,
    p(XSCoreParamsKey).VirtualLoadQueueSize, 1, 1
  )(p, QueueRTL)
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasVLSUParameters {
  val io = IO(new Bundle() {
    // control
    val redirect    = Flipped(Valid(new Redirect))
    val vecCommit   = Vec(VecLoadPipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO)))
    // from dispatch
    val enq         = new LqEnqIO
    // from ldu s3
    val ldin        = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new LqWriteBundle)))
    // to LoadQueueReplay and LoadQueueRAR
    val ldWbPtr     = Output(new LqPtr)
    // global
    val lqFull      = Output(Bool())
    val lqEmpty     = Output(Bool())
    // to dispatch
    val lqDeq       = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))

    val lqDeqRobIdx = Output(new RobPtr)
    val lqDeqUopIdx = Output(UopIdx())
  })

  println("VirtualLoadQueue: size: " + VirtualLoadQueueSize)
  //  VirtualLoadQueue field
  //  +-----------+---------+-------+
  //  | Allocated | MicroOp | Flags |
  //  +-----------+---------+-------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  Flags       : load flags
  override protected def initContents: Option[Seq[VirtualLoadQueuePayload]] =
    Some(Seq.fill(entries)(new VirtualLoadQueuePayload().Lit(
      _.valid -> false.B,
      _.isvec -> false.B
    )))

  // Read-only legacy views over the inherited payload storage.
  val allocated = Wire(Vec(VirtualLoadQueueSize, Bool()))
  val robIdx = Wire(Vec(VirtualLoadQueueSize, new RobPtr))
  val uopIdx = Wire(Vec(VirtualLoadQueueSize, UopIdx()))
  val isvec = Wire(Vec(VirtualLoadQueueSize, Bool()))
  val committed = Wire(Vec(VirtualLoadQueueSize, Bool()))
  val debug_mmio = Wire(Vec(VirtualLoadQueueSize, Bool()))
  val debug_paddr = Wire(Vec(VirtualLoadQueueSize, UInt(PAddrBits.W)))
  for (i <- 0 until VirtualLoadQueueSize) {
    val e = read(i)
    allocated(i) := e.valid
    robIdx(i) := e.robIdx
    uopIdx(i) := e.uopIdx
    isvec(i) := e.isvec
    committed(i) := e.committed
    debug_mmio(i) := e.debug_mmio
    debug_paddr(i) := e.debug_paddr
  }

  //  maintain pointers: inherited endpoints hold lane 0; other lanes are affine.
  val enqPtrExt = Wire(Vec(io.enq.req.length, new LqPtr))
  for (i <- 0 until enqPtrExt.length) {
    val lane = enqPtr + i.U
    enqPtrExt(i).flag := lane.flag
    enqPtrExt(i).value := lane.value
  }
  val enqPtrValue = enqPtr.value
  val deqPtrView = Wire(new LqPtr)
  deqPtrView.flag := deqPtr.flag
  deqPtrView.value := deqPtr.value
  val deqPtrNext = Wire(new LqPtr)

  /**
   * update pointer
   */
  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  val validCount = distanceBetween(enqPtrExt(0), deqPtrView)
  val allowEnqueue = validCount <= (VirtualLoadQueueSize - LSQLdEnqWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val needCancel = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => {
    robIdx(i).needFlush(io.redirect) && allocated(i)
  })))
  val lastNeedCancel = GatedValidRegNext(needCancel)
  val enqCancel = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancel.zip(vLoadFlow).map{case (v, flow) =>
    Mux(v, flow, 0.U)
  }
  val lastEnqCancel = GatedRegNext(enqCancelNum.reduce(_ + _))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect.valid)

  // update enqueue pointer
  val validVLoadFlow = vLoadFlow.zipWithIndex.map{case (vLoadFlowNumItem, index) => Mux(canEnqueue(index), vLoadFlowNumItem, 0.U)}
  val validVLoadOffset = vLoadFlow.zip(io.enq.needAlloc).map{case (flow, needAllocItem) => Mux(needAllocItem, flow, 0.U)}
  val validVLoadOffsetRShift = 0.U +: validVLoadOffset.take(validVLoadFlow.length - 1)

  val enqNumber = validVLoadFlow.reduce(_ + _)
  val enqPtrExtNextVec = Wire(Vec(io.enq.req.length, new LqPtr))
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new LqPtr))
  when (lastLastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  } .otherwise {
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect.valid && enqNumber =/= 0.U))

  when (isAfter(enqPtrExtNextVec(0), deqPtrNext)) {
    enqPtrExtNext := enqPtrExtNextVec
  } .otherwise {
    enqPtrExtNext := VecInit((0 until io.enq.req.length).map(i => deqPtrNext + i.U))
  }
  // Redirect recovery and the deq clamp install an absolute tail, not a count step.
  enqPtr.flag := enqPtrExtNext(0).flag
  enqPtr.value := enqPtrExtNext(0).value

  // update dequeue pointer
  val DeqPtrMoveStride = CommitWidth
  require(DeqPtrMoveStride == CommitWidth, "DeqPtrMoveStride must be equal to CommitWidth!")
  val deqLookupVec = VecInit((0 until DeqPtrMoveStride).map(deqPtrView + _.U))
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value) && committed(ptr.value) && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W))
  deqCountMask := deqLookup.asUInt & (~deqInSameRedirectCycle.asUInt).asUInt
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = GatedRegNext(commitCount)

  // update deqPtr
  // lastCommitCount was registered from cycle 1; deq consumes it at this edge.
  // deqPtrNext remains the next head used by the tail clamp.
  deqPtrNext := deqPtrView + lastCommitCount
  deq(lastCommitCount)

  io.lqDeq := GatedRegNext(lastCommitCount)
  io.lqCancelCnt := redirectCancelCount
  io.ldWbPtr := deqPtrView
  io.lqEmpty := RegNext(validCount === 0.U)

  io.lqDeqRobIdx := robIdx(deqPtrView.value)
  io.lqDeqUopIdx := uopIdx(deqPtrView.value)

  /**
   * Enqueue at dispatch
   *
   * Currently, VirtualLoadQueue only allows enqueue when #emptyEntries > EnqWidth
   * Dynamic enq based on numLsElem number
   */
  io.enq.canAccept := allowEnqueue
  val enqLowBound = io.enq.req.map(_.bits.lqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.lqIdx + x.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}

  for(i <- 0 until VirtualLoadQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }
    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    val enqEntry = Wire(new VirtualLoadQueuePayload)
    enqEntry.valid := true.B
    enqEntry.robIdx := selectBits.robIdx
    enqEntry.uopIdx := selectBits.uopIdx
    enqEntry.isvec := FuType.isVLoad(selectBits.fuType)
    enqEntry.committed := false.B
    enqEntry.debug_mmio := false.B
    enqEntry.debug_paddr := 0.U
    write(i.U, enqEntry, entryCanEnq)
  }

  for (i <- 0 until io.enq.req.length) {
    val lqIdx = enqPtrExt(0) + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.lqIdx
    XSError(canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.sqCanAccept), s"must accept $i\n")
    XSError(canEnqueue(i) && !enqCancel(i) && index.value =/= lqIdx.value, s"must be the same entry $i\n")
    io.enq.resp(i) := lqIdx
  }

  /**
    * Load commits
    *
    * When load commited, mark it as !allocated and move deqPtr forward.
    */
  (0 until DeqPtrMoveStride).map(i => {
    val slot = (deqPtrView + i.U).value
    writeField(slot, _.valid, false.B, commitCount > i.U)
    XSError(commitCount > i.U && !allocated(slot), s"why commit invalid entry $i?\n")
  })

  // vector commit or replay
  val vecLdCommittmp = Wire(Vec(VirtualLoadQueueSize, Vec(VecLoadPipelineWidth, Bool())))
  val vecLdCommit = Wire(Vec(VirtualLoadQueueSize, Bool()))
  for (i <- 0 until VirtualLoadQueueSize) {
    val cmt = io.vecCommit
    for (j <- 0 until VecLoadPipelineWidth) {
      vecLdCommittmp(i)(j) := allocated(i) && cmt(j).valid && robIdx(i) === cmt(j).bits.robidx && uopIdx(i) === cmt(j).bits.uopidx
    }
    vecLdCommit(i) := vecLdCommittmp(i).reduce(_ || _)

    writeField(i.U, _.committed, true.B, vecLdCommit(i) && isvec(i))
  }

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  for (i <- 0 until VirtualLoadQueueSize) {
    writeField(i.U, _.valid, false.B, needCancel(i))
  }

  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For ready load instruction (no need replay), it writes back to ROB immediately.
    */
  for(i <- 0 until LoadPipelineWidth) {
    //   most lq status need to be updated immediately after load writeback to lq
    //   flag bits in lq needs to be updated accurately
    io.ldin(i).ready := true.B
    val loadWbIndex = io.ldin(i).bits.uop.lqIdx.value

    val need_rep = io.ldin(i).bits.rep_info.need_rep
    val need_valid = io.ldin(i).bits.updateAddrValid
    val wbEnable = io.ldin(i).valid && !need_rep && need_valid && !io.ldin(i).bits.isvec
    writeField(loadWbIndex, _.committed, true.B, wbEnable)
    //  Debug info
    writeField(loadWbIndex, _.debug_mmio, io.ldin(i).bits.mmio, wbEnable)
    writeField(loadWbIndex, _.debug_paddr, io.ldin(i).bits.paddr, wbEnable)
    XSInfo(io.ldin(i).valid && !need_rep && need_valid,
      "load hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x isvec %x\n",
      io.ldin(i).bits.uop.lqIdx.asUInt,
      io.ldin(i).bits.uop.pc,
      io.ldin(i).bits.vaddr,
      io.ldin(i).bits.paddr,
      io.ldin(i).bits.mask,
      io.ldin(i).bits.forwardData.asUInt,
      io.ldin(i).bits.forwardMask.asUInt,
      io.ldin(i).bits.mmio,
      io.ldin(i).bits.isvec
    )
  }

  //  perf counter
  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  val vecValidVec = WireInit(VecInit((0 until VirtualLoadQueueSize).map(i => allocated(i) && isvec(i))))
  QueuePerf(VirtualLoadQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.lqFull := !allowEnqueue

  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtrValue, deqPtrView.flag, deqPtrView.value)

  def PrintFlag(flag: Bool, name: String): Unit = {
    XSDebug(false, flag, name) // when(flag)
    XSDebug(false, !flag, " ") // otherwise
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && committed(i), "c")
    PrintFlag(allocated(i) && isvec(i), "v")
    XSDebug(false, true.B, "\n")
  }
  // end

  // Slots are written by absolute lqIdx; no RingQueue-managed enqueue is used.
  enq(Seq(0.U.asTypeOf(new VirtualLoadQueuePayload)), Seq(false.B))
  emitRTL()
}
