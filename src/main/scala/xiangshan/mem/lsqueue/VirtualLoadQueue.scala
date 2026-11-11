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
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import utils._
import utils.QueueModel.{QueueRTL, RingQueue}
import utility._
import xiangshan._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._

class VirtualLoadQueuePayload(implicit p: Parameters) extends XSBundle {
  val valid       = Bool()
  val robIdx      = new RobPtr
  val uopIdx      = UopIdx()
  val isvec       = Bool()
  val committed   = Bool()
}

class VirtualLoadQueue(implicit p: Parameters)
  extends RingQueue[VirtualLoadQueuePayload](
    new VirtualLoadQueuePayload,
    p(XSCoreParamsKey).VirtualLoadQueueSize,
    p(XSCoreParamsKey).RenameWidth,
    p(XSCoreParamsKey).CommitWidth
  )(p, QueueRTL)
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
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val lqDeqRobIdx = Output(new RobPtr)
    val lqDeqUopIdx = Output(UopIdx())
  })

  println("VirtualLoadQueue: size: " + VirtualLoadQueueSize)

  override protected def initContents: Option[Seq[VirtualLoadQueuePayload]] =
    Some(Seq.fill(entries)(new VirtualLoadQueuePayload().Lit(
      _.valid -> false.B,
      _.isvec -> false.B
    )))

  val validCount = count
  val allowEnqueue = validCount <= (VirtualLoadQueueSize - LSQLdEnqWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))

  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  private def needsCancel(entry: VirtualLoadQueuePayload): Bool =
    entry.valid && entry.robIdx.needFlush(io.redirect)

  val needCancel = VecInit((0 until VirtualLoadQueueSize).map { i =>
    needsCancel(read(i))
  })
  val lastNeedCancel = GatedValidRegNext(needCancel)
  val enqCancel = canEnqueue.zip(io.enq.req).map { case (valid, request) =>
    valid && request.bits.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancel.zip(vLoadFlow).map { case (cancel, flow) =>
    Mux(cancel, flow, 0.U)
  }
  val lastEnqCancel = GatedRegNext(enqCancelNum.reduce(_ + _))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val redirectCancelCount = RegEnable(
    lastCycleCancelCount + lastEnqCancel,
    0.U,
    lastCycleRedirect.valid
  )

  val validVLoadFlow = vLoadFlow.zipWithIndex.map { case (flow, index) =>
    Mux(canEnqueue(index), flow, 0.U)
  }
  val validVLoadOffset = vLoadFlow.zip(io.enq.needAlloc).map { case (flow, needAlloc) =>
    Mux(needAlloc, flow, 0.U)
  }
  val validVLoadOffsetRShift = 0.U +: validVLoadOffset.take(validVLoadFlow.length - 1)
  val enqNumber = validVLoadFlow.reduce(_ + _)

  val deqLookupVec = VecInit((0 until CommitWidth).map(offset => deqPtr + offset.U))
  val deqCountMask = Wire(UInt(CommitWidth.W))
  val commitCount = Wire(UInt(log2Ceil(CommitWidth + 1).W))
  // Invalidation precedes the corresponding head advance by one cycle.
  val lastCommitCount = GatedRegNext(commitCount)
  val headEntries = deq(lastCommitCount, (0 until CommitWidth).map(_.U))
  val deqLookup = VecInit(headEntries.zip(deqLookupVec).map { case (entry, ptr) =>
    entry.valid && entry.committed && ptr =/= enqPtr
  })
  val deqInSameRedirectCycle = VecInit(headEntries.map(needsCancel))
  deqCountMask := deqLookup.asUInt & (~deqInSameRedirectCycle.asUInt).asUInt
  commitCount := PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)

  val deqPtrAfterCommit = deqPtr + lastCommitCount
  val naturalTail = Wire(new QueuePtr)
  when (lastLastCycleRedirect.valid) {
    naturalTail := enqPtr - redirectCancelCount
  }.otherwise {
    naturalTail := enqPtr + enqNumber
  }
  val clampTail = !isAfter(naturalTail, deqPtrAfterCommit)
  recover := lastLastCycleRedirect.valid || clampTail
  recoverPtr.flag := Mux(clampTail, deqPtrAfterCommit.flag, naturalTail.flag)
  recoverPtr.value := Mux(clampTail, deqPtrAfterCommit.value, naturalTail.value)
  // Cancelled flows still reserve slots; payload writes retain their supplied ranges.
  advanceEnq(enqNumber)

  assert(!(lastCycleRedirect.valid && enqNumber =/= 0.U))

  io.enq.canAccept := allowEnqueue
  io.lqDeq := GatedRegNext(lastCommitCount)
  io.lqCancelCnt := redirectCancelCount
  io.ldWbPtr.flag := deqPtr.flag
  io.ldWbPtr.value := deqPtr.value
  io.lqEmpty := RegNext(validCount === 0.U)
  io.lqDeqRobIdx := headEntries.head.robIdx
  io.lqDeqUopIdx := headEntries.head.uopIdx

  val enqLowBound = io.enq.req.map(_.bits.lqIdx)
  val enqUpBound = io.enq.req.map(request => request.bits.lqIdx + request.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map { case (low, up) =>
    low.flag =/= up.flag
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }
    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selected = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    val entry = Wire(new VirtualLoadQueuePayload)
    entry.valid := true.B
    entry.robIdx := selected.robIdx
    entry.uopIdx := selected.uopIdx
    entry.isvec := FuType.isVLoad(selected.fuType)
    entry.committed := false.B
    write(i.U, entry, entryCanEnq)
  }

  for (i <- io.enq.req.indices) {
    val responsePtr = enqPtr + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.lqIdx
    XSError(
      canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.sqCanAccept),
      s"must accept $i\n"
    )
    XSError(
      canEnqueue(i) && !enqCancel(i) && index.value =/= responsePtr.value,
      s"must be the same entry $i\n"
    )
    io.enq.resp(i).flag := responsePtr.flag
    io.enq.resp(i).value := responsePtr.value
  }

  for (i <- 0 until CommitWidth) {
    val slot = (deqPtr + i.U).value
    val commitEnable = commitCount > i.U
    writeField(slot, _.valid, false.B, commitEnable)
    XSError(commitEnable && !headEntries(i).valid, s"why commit invalid entry $i?\n")
  }

  val vecLdCommit = Wire(Vec(VirtualLoadQueueSize, Bool()))
  for (i <- 0 until VirtualLoadQueueSize) {
    val entry = read(i)
    val matches = io.vecCommit.map { commit =>
      commit.valid && entry.robIdx === commit.bits.robidx && entry.uopIdx === commit.bits.uopidx
    }
    vecLdCommit(i) := entry.valid && matches.reduce(_ || _)
    writeField(i.U, _.committed, true.B, vecLdCommit(i) && entry.isvec)
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    writeField(i.U, _.valid, false.B, needCancel(i))
  }

  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  for (i <- 0 until LoadPipelineWidth) {
    io.ldin(i).ready := true.B
    val loadWbIndex = io.ldin(i).bits.uop.lqIdx.value
    val needReplay = io.ldin(i).bits.rep_info.need_rep
    val needValid = io.ldin(i).bits.updateAddrValid
    val writeback = io.ldin(i).valid && !needReplay && needValid && !io.ldin(i).bits.isvec
    writeField(loadWbIndex, _.committed, true.B, writeback)
    XSInfo(io.ldin(i).valid && !needReplay && needValid,
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

  QueuePerf(VirtualLoadQueueSize, validCount, !allowEnqueue)
  val vecValidCount = PopCount((0 until VirtualLoadQueueSize).map { i =>
    val entry = read(i)
    entry.valid && entry.isvec
  })
  QueuePerf(VirtualLoadQueueSize, vecValidCount, !allowEnqueue)
  io.lqFull := !allowEnqueue

  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtr.flag, enqPtr.value, deqPtr.flag, deqPtr.value)

  def printFlag(flag: Bool, name: String): Unit = {
    XSDebug(false, flag, name)
    XSDebug(false, !flag, " ")
  }

  for (i <- 0 until VirtualLoadQueueSize) {
    val entry = read(i)
    printFlag(entry.valid, "a")
    printFlag(entry.valid && entry.committed, "c")
    printFlag(entry.valid && entry.isvec, "v")
    XSDebug(false, true.B, "\n")
  }
}
