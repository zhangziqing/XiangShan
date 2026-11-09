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
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, UopIdx}
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._
import scala.util.Random

/** Test-only view of the per-entry ownership state. */
class VLoadQueueDbg(implicit p: Parameters) extends XSBundle {
  val allocated   = Vec(VirtualLoadQueueSize, Bool())
  val robIdx      = Vec(VirtualLoadQueueSize, new RobPtr)
  val uopIdx      = Vec(VirtualLoadQueueSize, UopIdx())
  val isvec       = Vec(VirtualLoadQueueSize, Bool())
  val committed   = Vec(VirtualLoadQueueSize, Bool())
  val debug_mmio  = Vec(VirtualLoadQueueSize, Bool())
  val debug_paddr = Vec(VirtualLoadQueueSize, UInt(PAddrBits.W))
}

/** Production VirtualLoadQueue with its internal state exposed for the miter. */
class TestableVirtualLoadQueue(implicit p: Parameters) extends VirtualLoadQueue {
  val dbg = IO(Output(new VLoadQueueDbg))
  for (i <- 0 until entries) {
    val entry = read(i)
    dbg.allocated(i) := entry.valid
    dbg.robIdx(i) := entry.robIdx
    dbg.uopIdx(i) := entry.uopIdx
    dbg.isvec(i) := entry.isvec
    dbg.committed(i) := entry.committed
    dbg.debug_mmio(i) := entry.debug_mmio
    dbg.debug_paddr(i) := entry.debug_paddr
  }
}

/** Verbatim legacy VirtualLoadQueue with the same test-only state exposure. */
class TestableLegacyVirtualLoadQueue(implicit p: Parameters) extends LegacyVirtualLoadQueue {
  val dbg = IO(Output(new VLoadQueueDbg))
  dbg.allocated   := allocated
  dbg.robIdx      := robIdx
  dbg.uopIdx      := uopIdx
  dbg.isvec       := isvec
  dbg.committed   := committed
  dbg.debug_mmio  := debug_mmio
  dbg.debug_paddr := debug_paddr
}

/** Miter harness: production VirtualLoadQueue beside the verbatim legacy copy.
  *
  * Field-by-field: public outputs plus the exposed internal state. Payload is
  * compared only while either side marks the entry allocated, so unreset
  * registers are never compared as zeros.
  */
class VLoadQueueMiterHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val initDefined = Input(Bool())
    val needAlloc  = Input(Vec(LSQEnqWidth, Bool()))
    val reqValid   = Input(Vec(LSQEnqWidth, Bool()))
    val reqRobVal  = Input(Vec(LSQEnqWidth, UInt(8.W)))
    val reqRobFlag = Input(Vec(LSQEnqWidth, Bool()))
    val reqUopIdx  = Input(Vec(LSQEnqWidth, UInt(8.W)))
    val reqLqVal   = Input(Vec(LSQEnqWidth, UInt(8.W)))
    val reqLqFlag  = Input(Vec(LSQEnqWidth, Bool()))
    val reqNlsElem = Input(Vec(LSQEnqWidth, UInt(8.W)))
    val reqFuType  = Input(Vec(LSQEnqWidth, UInt(xiangshan.backend.fu.FuType.num.W)))
    val sqCanAccept = Input(Bool())

    val redirectValid  = Input(Bool())
    val redirectRobVal = Input(UInt(8.W))
    val redirectRobFlag = Input(Bool())

    val ldinValid  = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinLqVal  = Input(Vec(LoadPipelineWidth, UInt(8.W)))
    val ldinLqFlag = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinNeedRep = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinUpdOk  = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinIsvec  = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinMmio   = Input(Vec(LoadPipelineWidth, Bool()))
    val ldinPaddr  = Input(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))

    val vcValid    = Input(Vec(VecLoadPipelineWidth, Bool()))
    val vcRobVal   = Input(Vec(VecLoadPipelineWidth, UInt(8.W)))
    val vcRobFlag  = Input(Vec(VecLoadPipelineWidth, Bool()))
    val vcUopIdx   = Input(Vec(VecLoadPipelineWidth, UInt(8.W)))

    val dbgIdx = Input(UInt(8.W))

    val canAccept  = Output(Bool())
    val legalLqIdx = Output(UInt())
    val legalLqFlag = Output(Bool())
    val newOut     = Output(UInt())
    val refOut     = Output(UInt())
    val mismatch   = Output(Bool())

    val dbgAllocated   = Output(Bool())
    val dbgCommitted   = Output(Bool())
    val dbgMmio        = Output(Bool())
    val dbgPaddr       = Output(UInt())
    val dbgIsvec       = Output(Bool())
  })

  private val newQ = Module(new TestableVirtualLoadQueue)
  private val refQ = Module(new TestableLegacyVirtualLoadQueue)

  private def driveRedirect(valid: Bool, robVal: UInt, robFlag: Bool): Redirect = {
    val r = Wire(new Redirect)
    r := 0.U.asTypeOf(new Redirect)
    r.robIdx.value := robVal
    r.robIdx.flag  := robFlag
    r
  }

  // ---- production instance wiring ----
  newQ.io.redirect.valid := io.redirectValid
  newQ.io.redirect.bits  := driveRedirect(io.redirectValid, io.redirectRobVal, io.redirectRobFlag)
  for (j <- 0 until VecLoadPipelineWidth) {
    newQ.io.vecCommit(j).valid := io.vcValid(j)
    newQ.io.vecCommit(j).bits  := 0.U.asTypeOf(new FeedbackToLsqIO)
    newQ.io.vecCommit(j).bits.robidx.value := io.vcRobVal(j)
    newQ.io.vecCommit(j).bits.robidx.flag  := io.vcRobFlag(j)
    newQ.io.vecCommit(j).bits.uopidx       := io.vcUopIdx(j)
  }
  newQ.io.enq.sqCanAccept := io.sqCanAccept
  for (j <- 0 until LSQEnqWidth) {
    newQ.io.enq.needAlloc(j) := io.needAlloc(j)
    newQ.io.enq.req(j).valid := io.reqValid(j)
    newQ.io.enq.req(j).bits  := 0.U.asTypeOf(new DynInst)
    newQ.io.enq.req(j).bits.robIdx.value := io.reqRobVal(j)
    newQ.io.enq.req(j).bits.robIdx.flag  := io.reqRobFlag(j)
    newQ.io.enq.req(j).bits.uopIdx       := io.reqUopIdx(j)
    newQ.io.enq.req(j).bits.lqIdx.value  := io.reqLqVal(j)
    newQ.io.enq.req(j).bits.lqIdx.flag   := io.reqLqFlag(j)
    newQ.io.enq.req(j).bits.numLsElem    := io.reqNlsElem(j)
    newQ.io.enq.req(j).bits.fuType       := io.reqFuType(j)
  }
  for (i <- 0 until LoadPipelineWidth) {
    newQ.io.ldin(i).valid := io.ldinValid(i)
    newQ.io.ldin(i).bits  := 0.U.asTypeOf(new LqWriteBundle)
    newQ.io.ldin(i).bits.uop.lqIdx.value   := io.ldinLqVal(i)
    newQ.io.ldin(i).bits.uop.lqIdx.flag    := io.ldinLqFlag(i)
    newQ.io.ldin(i).bits.rep_info.cause(0) := io.ldinNeedRep(i)
    newQ.io.ldin(i).bits.updateAddrValid   := io.ldinUpdOk(i)
    newQ.io.ldin(i).bits.isvec             := io.ldinIsvec(i)
    newQ.io.ldin(i).bits.mmio              := io.ldinMmio(i)
    newQ.io.ldin(i).bits.paddr             := io.ldinPaddr(i)
  }

  // ---- legacy reference instance wiring ----
  refQ.io.redirect.valid := io.redirectValid
  refQ.io.redirect.bits  := driveRedirect(io.redirectValid, io.redirectRobVal, io.redirectRobFlag)
  for (j <- 0 until VecLoadPipelineWidth) {
    refQ.io.vecCommit(j).valid := io.vcValid(j)
    refQ.io.vecCommit(j).bits  := 0.U.asTypeOf(new FeedbackToLsqIO)
    refQ.io.vecCommit(j).bits.robidx.value := io.vcRobVal(j)
    refQ.io.vecCommit(j).bits.robidx.flag  := io.vcRobFlag(j)
    refQ.io.vecCommit(j).bits.uopidx       := io.vcUopIdx(j)
  }
  refQ.io.enq.sqCanAccept := io.sqCanAccept
  for (j <- 0 until LSQEnqWidth) {
    refQ.io.enq.needAlloc(j) := io.needAlloc(j)
    refQ.io.enq.req(j).valid := io.reqValid(j)
    refQ.io.enq.req(j).bits  := 0.U.asTypeOf(new DynInst)
    refQ.io.enq.req(j).bits.robIdx.value := io.reqRobVal(j)
    refQ.io.enq.req(j).bits.robIdx.flag  := io.reqRobFlag(j)
    refQ.io.enq.req(j).bits.uopIdx       := io.reqUopIdx(j)
    refQ.io.enq.req(j).bits.lqIdx.value  := io.reqLqVal(j)
    refQ.io.enq.req(j).bits.lqIdx.flag   := io.reqLqFlag(j)
    refQ.io.enq.req(j).bits.numLsElem    := io.reqNlsElem(j)
    refQ.io.enq.req(j).bits.fuType       := io.reqFuType(j)
  }
  for (i <- 0 until LoadPipelineWidth) {
    refQ.io.ldin(i).valid := io.ldinValid(i)
    refQ.io.ldin(i).bits  := 0.U.asTypeOf(new LqWriteBundle)
    refQ.io.ldin(i).bits.uop.lqIdx.value   := io.ldinLqVal(i)
    refQ.io.ldin(i).bits.uop.lqIdx.flag    := io.ldinLqFlag(i)
    refQ.io.ldin(i).bits.rep_info.cause(0) := io.ldinNeedRep(i)
    refQ.io.ldin(i).bits.updateAddrValid   := io.ldinUpdOk(i)
    refQ.io.ldin(i).bits.isvec             := io.ldinIsvec(i)
    refQ.io.ldin(i).bits.mmio              := io.ldinMmio(i)
    refQ.io.ldin(i).bits.paddr             := io.ldinPaddr(i)
  }

  // Unreset payload definedness persists through mid-run reset. Initialization
  // is an explicit test-only pulse, independent of DUT reset.
  val entryDefined = Reg(Vec(VirtualLoadQueueSize, Bool()))
  val wbDefined = Reg(Vec(VirtualLoadQueueSize, Bool()))
  for (i <- 0 until VirtualLoadQueueSize) {
    val captures = newQ.io.enq.req.map { req =>
      val low = req.bits.lqIdx
      val high = low + req.bits.numLsElem
      val hit = Mux(low.flag =/= high.flag,
        low.value <= i.U || i.U < high.value,
        low.value <= i.U && i.U < high.value)
      req.valid && !req.bits.robIdx.needFlush(newQ.io.redirect) && hit
    }.reduce(_ || _)
    val writes = newQ.io.ldin.map { wb =>
      wb.valid && !wb.bits.rep_info.need_rep && wb.bits.updateAddrValid &&
        !wb.bits.isvec && wb.bits.uop.lqIdx.value === i.U
    }.reduce(_ || _)
    when(io.initDefined) { entryDefined(i) := false.B; wbDefined(i) := false.B }
      .otherwise {
        when(captures) { entryDefined(i) := true.B }
        when(writes) { wbDefined(i) := true.B }
      }
  }

  private def outputsNew: UInt = Cat(
    newQ.io.lqDeq, newQ.io.lqCancelCnt, newQ.io.lqFull, newQ.io.lqEmpty,
    newQ.io.ldWbPtr.asUInt,
    Mux(entryDefined(newQ.io.ldWbPtr.value), newQ.io.lqDeqRobIdx.asUInt, 0.U),
    Mux(entryDefined(newQ.io.ldWbPtr.value), newQ.io.lqDeqUopIdx, 0.U),
    newQ.io.enq.canAccept, VecInit(newQ.io.enq.resp.map(_.asUInt)).asUInt,
    VecInit(newQ.io.ldin.map(_.ready)).asUInt
  )
  private def outputsRef: UInt = Cat(
    refQ.io.lqDeq, refQ.io.lqCancelCnt, refQ.io.lqFull, refQ.io.lqEmpty,
    refQ.io.ldWbPtr.asUInt,
    Mux(entryDefined(refQ.io.ldWbPtr.value), refQ.io.lqDeqRobIdx.asUInt, 0.U),
    Mux(entryDefined(refQ.io.ldWbPtr.value), refQ.io.lqDeqUopIdx, 0.U),
    refQ.io.enq.canAccept, VecInit(refQ.io.enq.resp.map(_.asUInt)).asUInt,
    VecInit(refQ.io.ldin.map(_.ready)).asUInt
  )
  io.newOut := outputsNew
  io.refOut := outputsRef
  io.canAccept  := newQ.io.enq.canAccept
  io.legalLqIdx := newQ.io.enq.resp(0).value
  io.legalLqFlag := newQ.io.enq.resp(0).flag

  val allocMismatch = (0 until VirtualLoadQueueSize).map(i => newQ.dbg.allocated(i) ^ refQ.dbg.allocated(i)).reduce(_ || _)
  val payloadMismatch = (0 until VirtualLoadQueueSize).map { i =>
    val metaSame = newQ.dbg.robIdx(i).asUInt === refQ.dbg.robIdx(i).asUInt &&
      newQ.dbg.uopIdx(i) === refQ.dbg.uopIdx(i)
    val wbSame = newQ.dbg.committed(i) === refQ.dbg.committed(i) &&
      newQ.dbg.debug_mmio(i) === refQ.dbg.debug_mmio(i) &&
      newQ.dbg.debug_paddr(i) === refQ.dbg.debug_paddr(i)
    (!entryDefined(i) || metaSame) &&
      (!(entryDefined(i) || wbDefined(i)) || wbSame) &&
      newQ.dbg.isvec(i) === refQ.dbg.isvec(i)
  }.reduce(_ && _)
  io.mismatch := allocMismatch || !payloadMismatch

  io.dbgAllocated := newQ.dbg.allocated(io.dbgIdx)
  io.dbgCommitted := newQ.dbg.committed(io.dbgIdx)
  io.dbgMmio      := newQ.dbg.debug_mmio(io.dbgIdx)
  io.dbgPaddr     := newQ.dbg.debug_paddr(io.dbgIdx)
  io.dbgIsvec     := newQ.dbg.isvec(io.dbgIdx)
}

class VirtualLoadQueueSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  // Smallest legal depth so the caller miter (two full ownership tables plus the
  // legacy copy) stays fast; the real production class and all field ports are
  // kept and wrap/collision coverage is exercised more often.
  private val core = base(XSTileKey).head.copy(VirtualLoadQueueSize = 8)
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => core
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.CRITICAL,
      perfDBHartID = 0
    )
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform,
      here(DebugOptionsKey).EnableXMR
    )
  })

  private val lqSize = core.VirtualLoadQueueSize
  private val lsqEnqWidth = core.RenameWidth
  private val ldW = core.LoadPipelineWidth
  private val vecLdW = core.VecLoadPipelineWidth

  private def idleLdin(dut: VLoadQueueMiterHarness): Unit = {
    for (i <- 0 until ldW) {
      dut.io.ldinValid(i).poke(false.B)
      dut.io.ldinLqVal(i).poke(0.U)
      dut.io.ldinLqFlag(i).poke(false.B)
      dut.io.ldinNeedRep(i).poke(false.B)
      dut.io.ldinUpdOk(i).poke(false.B)
      dut.io.ldinIsvec(i).poke(false.B)
      dut.io.ldinMmio(i).poke(false.B)
      dut.io.ldinPaddr(i).poke(0.U)
    }
  }
  private def idleVec(dut: VLoadQueueMiterHarness): Unit = {
    for (j <- 0 until vecLdW) {
      dut.io.vcValid(j).poke(false.B)
      dut.io.vcRobVal(j).poke(0.U)
      dut.io.vcRobFlag(j).poke(false.B)
      dut.io.vcUopIdx(j).poke(0.U)
    }
  }
  private def idleReq(dut: VLoadQueueMiterHarness): Unit = {
    for (j <- 0 until lsqEnqWidth) {
      dut.io.needAlloc(j).poke(false.B)
      dut.io.reqValid(j).poke(false.B)
      dut.io.reqRobVal(j).poke(0.U)
      dut.io.reqRobFlag(j).poke(false.B)
      dut.io.reqUopIdx(j).poke(0.U)
      dut.io.reqLqVal(j).poke(0.U)
      dut.io.reqLqFlag(j).poke(false.B)
      dut.io.reqNlsElem(j).poke(1.U)
      dut.io.reqFuType(j).poke(0.U)
    }
  }
  private def idleRedirect(dut: VLoadQueueMiterHarness): Unit = {
    dut.io.redirectValid.poke(false.B)
    dut.io.redirectRobVal.poke(0.U)
    dut.io.redirectRobFlag.poke(false.B)
  }

  it should "match the legacy VirtualLoadQueue under legal random ownership traffic" in {
    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      idleReq(dut); idleLdin(dut); idleVec(dut); idleRedirect(dut)
      dut.io.sqCanAccept.poke(true.B); dut.io.dbgIdx.poke(0.U)
      dut.io.initDefined.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(4); dut.reset.poke(false.B)
      dut.io.initDefined.poke(false.B)
      dut.io.dbgIdx.poke(0.U)

      val rng = new Random(0x5EED)
      var redirectPause = 0
      for (cycle <- 0 until 4000) {
        if (redirectPause > 0) redirectPause -= 1
        // Legal dispatch: lane 0 only, gated by canAccept, index from resp.
        idleReq(dut)
        val canAcc = dut.io.canAccept.peek().litValue != 0
        val dispatch = canAcc && redirectPause == 0 && rng.nextInt(3) != 0
        dut.io.needAlloc(0).poke(dispatch.B)
        dut.io.reqValid(0).poke(dispatch.B)
        dut.io.reqRobVal(0).poke(rng.nextInt(64).U)
        dut.io.reqRobFlag(0).poke(rng.nextBoolean().B)
        dut.io.reqUopIdx(0).poke(rng.nextInt(8).U)
        dut.io.reqNlsElem(0).poke(1.U)
        dut.io.reqFuType(0).poke(0.U)
        dut.io.reqLqVal(0).poke(dut.io.legalLqIdx.peek().litValue.U)
        dut.io.reqLqFlag(0).poke((dut.io.legalLqFlag.peek().litValue != 0).B)

        // Redirect a fraction of cycles (level 0 => flush strictly after ptr).
        val redir = redirectPause == 0 && rng.nextInt(40) == 0
        if (redir) redirectPause = 4
        dut.io.redirectValid.poke(redir.B)
        dut.io.redirectRobVal.poke(rng.nextInt(64).U)
        dut.io.redirectRobFlag.poke(rng.nextBoolean().B)

        // Writeback lanes target real entries only while a load is in flight.
        for (i <- 0 until ldW) {
          val en = (rng.nextInt(4) != 0)
          dut.io.ldinValid(i).poke(en.B)
          dut.io.ldinLqVal(i).poke(rng.nextInt(lqSize).U)
          dut.io.ldinLqFlag(i).poke(false.B)
          dut.io.ldinNeedRep(i).poke(rng.nextBoolean().B)
          dut.io.ldinUpdOk(i).poke(rng.nextBoolean().B)
          dut.io.ldinIsvec(i).poke(false.B)
          dut.io.ldinMmio(i).poke(rng.nextBoolean().B)
          dut.io.ldinPaddr(i).poke(rng.nextInt(1 << 20).U)
        }
        idleVec(dut)
        for (j <- 0 until vecLdW) {
          dut.io.vcValid(j).poke((rng.nextInt(4) != 0).B)
          dut.io.vcRobVal(j).poke(rng.nextInt(64).U)
          dut.io.vcRobFlag(j).poke(rng.nextBoolean().B)
          dut.io.vcUopIdx(j).poke(rng.nextInt(8).U)
        }

        val nv = dut.io.newOut.peek().litValue
        val rv = dut.io.refOut.peek().litValue
        assert(nv == rv, s"public output mismatch at cycle $cycle: new=0x${nv.toString(16)} ref=0x${rv.toString(16)}")
        assert(dut.io.mismatch.peek().litValue == 0, s"internal ownership mismatch at cycle $cycle")
        dut.clock.step(1)
        assert(dut.io.newOut.peek().litValue == dut.io.refOut.peek().litValue, s"post-edge public output at $cycle")
        dut.io.mismatch.expect(false.B)
      }
    }
  }

  it should "give the later writeback lane priority at the same index" in {
    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      idleReq(dut); idleLdin(dut); idleVec(dut); idleRedirect(dut)
      dut.io.sqCanAccept.poke(true.B); dut.io.dbgIdx.poke(0.U)
      dut.io.initDefined.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(4); dut.reset.poke(false.B)
      dut.io.initDefined.poke(false.B)

      // Enqueue lane 0 legally and capture its entry index.
      dut.io.needAlloc(0).poke(true.B); dut.io.reqValid(0).poke(true.B)
      dut.io.reqRobVal(0).poke(5.U); dut.io.reqRobFlag(0).poke(false.B)
      dut.io.reqUopIdx(0).poke(1.U); dut.io.reqNlsElem(0).poke(1.U); dut.io.reqFuType(0).poke(0.U)
      dut.io.reqLqVal(0).poke(dut.io.legalLqIdx.peek().litValue.U)
        dut.io.reqLqFlag(0).poke((dut.io.legalLqFlag.peek().litValue != 0).B)
      val idx = dut.io.legalLqIdx.peek().litValue.toInt
      dut.io.dbgIdx.poke(idx.U)
      dut.clock.step(1)
      dut.io.needAlloc(0).poke(false.B); dut.io.reqValid(0).poke(false.B)

      // Two scalar writebacks to the same entry with distinct paddr/mmio:
      // lane 1 (later) must win for both committed and debug fields.
      idleLdin(dut)
      for (i <- 0 until ldW) {
        dut.io.ldinValid(i).poke(true.B)
        dut.io.ldinLqVal(i).poke(idx.U)
        dut.io.ldinNeedRep(i).poke(false.B)
        dut.io.ldinUpdOk(i).poke(true.B)
        dut.io.ldinIsvec(i).poke(false.B)
        dut.io.ldinMmio(i).poke((i == ldW - 1).B)
        dut.io.ldinPaddr(i).poke((0x1000 + i).U)
      }
      dut.clock.step(1)
      idleLdin(dut)
      dut.io.dbgAllocated.expect(true.B)
      dut.io.dbgCommitted.expect(true.B)
      dut.io.dbgMmio.expect(true.B) // later lane sets mmio
      dut.io.dbgPaddr.expect((0x1000 + ldW - 1).U) // later lane paddr
      dut.io.mismatch.expect(false.B)
      assert(dut.io.newOut.peek().litValue == dut.io.refOut.peek().litValue)
    }
  }
  it should "match complete ring wrap, retirement and unreset state through reset" in {
    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      idleReq(dut); idleLdin(dut); idleVec(dut); idleRedirect(dut)
      dut.io.sqCanAccept.poke(true.B); dut.io.dbgIdx.poke(0.U)
      dut.io.initDefined.poke(true.B); dut.reset.poke(true.B); dut.clock.step(2)
      dut.io.initDefined.poke(false.B); dut.reset.poke(false.B)
      def edge(): Unit = {
        dut.clock.step(1)
        dut.io.mismatch.expect(false.B)
        assert(dut.io.newOut.peek().litValue == dut.io.refOut.peek().litValue)
      }
      for (round <- 0 until 24) {
        dut.io.canAccept.expect(true.B)
        val idx = dut.io.legalLqIdx.peek().litValue
        idleReq(dut)
        dut.io.needAlloc(0).poke(true.B); dut.io.reqValid(0).poke(true.B)
        dut.io.reqRobVal(0).poke((round + 10).U); dut.io.reqUopIdx(0).poke((round % 8).U)
        dut.io.reqLqVal(0).poke(idx.U)
        dut.io.reqLqFlag(0).poke((dut.io.legalLqFlag.peek().litValue != 0).B)
        edge(); idleReq(dut)
        dut.io.ldinValid(0).poke(true.B); dut.io.ldinLqVal(0).poke(idx.U)
        dut.io.ldinUpdOk(0).poke(true.B); dut.io.ldinPaddr(0).poke((0x1000 + round).U)
        edge(); idleLdin(dut)
        for (_ <- 0 until 4) edge()
      }
      // An enabled payload write survives a reset edge although validity resets.
      idleReq(dut)
      dut.io.needAlloc(0).poke(true.B); dut.io.reqValid(0).poke(true.B)
      dut.io.reqRobVal(0).poke(99.U); dut.io.reqLqVal(0).poke(dut.io.legalLqIdx.peek().litValue.U)
      dut.io.reqLqFlag(0).poke((dut.io.legalLqFlag.peek().litValue != 0).B)
      dut.reset.poke(true.B); edge(); idleReq(dut); edge()
      dut.reset.poke(false.B); edge(); edge()
    }
  }

}
