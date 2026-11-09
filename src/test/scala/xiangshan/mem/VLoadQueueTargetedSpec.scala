// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Focused candidate/legacy miter coverage for the native VLQ enqueue mapping.
  *
  * Lane 0 only: the harness exposes permit-legal lane-0 lqIdx. This keeps the
  * driver independent of a software pointer model while exercising variable
  * numLsElem ranges, canceled dispatch, two-cycle rollback, wrapping and
  * allocation/writeback same-slot conflicts at size 8 and non-power-of-two 72.
  */
class VLoadQueueTargetedSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def params(size: Int): Parameters = {
    val base = new DefaultConfig
    val core = base(XSTileKey).head.copy(VirtualLoadQueueSize = size)
    base.alterPartial({
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
  }

  private def idleLdin(dut: VLoadQueueMiterHarness, ldW: Int): Unit = {
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

  private def idleVec(dut: VLoadQueueMiterHarness, vecW: Int): Unit = {
    for (j <- 0 until vecW) {
      dut.io.vcValid(j).poke(false.B)
      dut.io.vcRobVal(j).poke(0.U)
      dut.io.vcRobFlag(j).poke(false.B)
      dut.io.vcUopIdx(j).poke(0.U)
    }
  }

  private def idleReq(dut: VLoadQueueMiterHarness, enqW: Int): Unit = {
    for (j <- 0 until enqW) {
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

  private def dispatch(
    dut: VLoadQueueMiterHarness, lane: Int, robVal: Int, robFlag: Boolean,
    nls: Int, lqVal: Int, lqFlag: Boolean
  ): Unit = {
    dut.io.needAlloc(lane).poke(true.B)
    dut.io.reqValid(lane).poke(true.B)
    dut.io.reqRobVal(lane).poke(robVal.U)
    dut.io.reqRobFlag(lane).poke(robFlag.B)
    dut.io.reqUopIdx(lane).poke((robVal & 7).U)
    dut.io.reqLqVal(lane).poke(lqVal.U)
    dut.io.reqLqFlag(lane).poke(lqFlag.B)
    dut.io.reqNlsElem(lane).poke(nls.U)
    dut.io.reqFuType(lane).poke(0.U)
  }

  private def writeback(
    dut: VLoadQueueMiterHarness, lane: Int, value: Int, flag: Boolean,
    mmio: Boolean, paddr: Int
  ): Unit = {
    dut.io.ldinValid(lane).poke(true.B)
    dut.io.ldinLqVal(lane).poke(value.U)
    dut.io.ldinLqFlag(lane).poke(flag.B)
    dut.io.ldinNeedRep(lane).poke(false.B)
    dut.io.ldinUpdOk(lane).poke(true.B)
    dut.io.ldinIsvec(lane).poke(false.B)
    dut.io.ldinMmio(lane).poke(mmio.B)
    dut.io.ldinPaddr(lane).poke(paddr.U)
  }

  private def check(dut: VLoadQueueMiterHarness, tag: String, cycle: Int): Unit = {
    val nv = dut.io.newOut.peek().litValue
    val rv = dut.io.refOut.peek().litValue
    assert(nv == rv, s"$tag cycle $cycle public mismatch: new=$nv ref=$rv")
    assert(dut.io.mismatch.peek().litValue == 0, s"$tag cycle $cycle ownership mismatch")
  }

  private def resetDut(dut: VLoadQueueMiterHarness, enqW: Int, ldW: Int, vecW: Int): Unit = {
    idleReq(dut, enqW); idleLdin(dut, ldW); idleVec(dut, vecW); idleRedirect(dut)
    dut.io.sqCanAccept.poke(true.B)
    dut.io.dbgIdx.poke(0.U)
    dut.io.initDefined.poke(true.B)
    dut.reset.poke(true.B)
    dut.clock.step(4)
    dut.reset.poke(false.B)
    dut.io.initDefined.poke(false.B)
    dut.io.dbgIdx.poke(0.U)
  }

  private def slotOf(start: Int, k: Int, size: Int): Int = (start + k) % size
  private def slotFlag(startFlag: Boolean, start: Int, k: Int, size: Int): Boolean =
    startFlag ^ (start + k >= size)

  private def waitCanAccept(
    dut: VLoadQueueMiterHarness, enqW: Int, ldW: Int, vecW: Int, tag: String, maxCycles: Int = 40
  ): Unit = {
    var n = 0
    while (dut.io.canAccept.peek().litValue == 0 && n < maxCycles) {
      idleReq(dut, enqW); idleLdin(dut, ldW); idleVec(dut, vecW); idleRedirect(dut)
      dut.clock.step(1)
      check(dut, tag + "-wait", n)
      n += 1
    }
    assert(dut.io.canAccept.peek().litValue != 0, s"$tag: admission did not drain")
  }

  it should "cover variable flow, cancel, rollback and same-cycle writeback priority at size 8" in {
    implicit val p: Parameters = params(8)
    val size = 8
    val enqW = p(XSCoreParamsKey).RenameWidth
    val ldW = p(XSCoreParamsKey).LoadPipelineWidth
    val vecW = p(XSCoreParamsKey).VecLoadPipelineWidth

    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      resetDut(dut, enqW, ldW, vecW)

      // Four-slot range, with a same-cycle writeback to the first allocated slot.
      dut.io.canAccept.expect(true.B)
      val start0 = dut.io.legalLqIdx.peek().litValue.toInt
      val flag0 = dut.io.legalLqFlag.peek().litValue != 0
      idleReq(dut, enqW)
      dispatch(dut, 0, robVal = 20, robFlag = false, nls = 4, lqVal = start0, lqFlag = flag0)
      idleLdin(dut, ldW)
      writeback(dut, 0, start0, flag0, mmio = true, paddr = 0x100)
      check(dut, "alloc-conflict-before", 0)
      dut.clock.step(1)
      idleReq(dut, enqW); idleLdin(dut, ldW)
      check(dut, "alloc-conflict-after", 0)

      // Retire the range; writeback has to see the allocation version on the
      // first slot and the remaining slots individually.
      for (k <- 0 until 4) {
        idleLdin(dut, ldW)
        writeback(dut, 0, slotOf(start0, k, size), slotFlag(flag0, start0, k, size), false, 0x200 + k)
        check(dut, "retire-before", k)
        dut.clock.step(1)
        idleLdin(dut, ldW)
        check(dut, "retire-after", k)
      }
      waitCanAccept(dut, enqW, ldW, vecW, "after-retire")

      // Canceled dispatch: reservation advances then rolls back without writes.
      val cstart = dut.io.legalLqIdx.peek().litValue.toInt
      val cflag = dut.io.legalLqFlag.peek().litValue != 0
      idleReq(dut, enqW); idleLdin(dut, ldW); idleVec(dut, vecW); idleRedirect(dut)
      dispatch(dut, 0, robVal = 10, robFlag = false, nls = 4, lqVal = cstart, lqFlag = cflag)
      dut.io.redirectValid.poke(true.B)
      dut.io.redirectRobVal.poke(9.U)
      dut.io.redirectRobFlag.poke(false.B)
      check(dut, "cancel-dispatch-before", 0)
      dut.clock.step(1)
      idleReq(dut, enqW); idleLdin(dut, ldW); idleRedirect(dut)
      check(dut, "cancel-dispatch-after", 0)
      for (k <- 0 until 4) {
        dut.clock.step(1)
        check(dut, "cancel-rollback", k)
      }
      waitCanAccept(dut, enqW, ldW, vecW, "after-cancel")

      // Random lane-0 traffic with variable flow, canceled dispatch and
      // redirects. Writebacks target previously allocated slots, including
      // free slots and current tail slots, so allocation/writeback priority is
      // exercised repeatedly.
      val rng = new Random(0x5111)
      val known = ArrayBuffer.empty[(Int, Boolean)]
      var wbIndex = 0
      var pause = 0
      var canceled = 0
      var redirects = 0
      for (cycle <- 0 until 500) {
        if (pause > 0) pause -= 1
        idleReq(dut, enqW); idleLdin(dut, ldW); idleVec(dut, vecW); idleRedirect(dut)
        val can = dut.io.canAccept.peek().litValue != 0
        val doRedirect = pause == 0 && rng.nextInt(17) == 0
        var doDispatch = can && pause == 0 && rng.nextInt(3) != 0
        var robVal = rng.nextInt(16)
        var canceledNow = false
        if (doRedirect) {
          val redirVal = rng.nextInt(8)
          dut.io.redirectValid.poke(true.B)
          dut.io.redirectRobVal.poke(redirVal.U)
          dut.io.redirectRobFlag.poke(false.B)
          pause = 3
          redirects += 1
          if (can) {
            doDispatch = true
            robVal = redirVal + 1
            canceledNow = true
            canceled += 1
          }
        }
        val start = dut.io.legalLqIdx.peek().litValue.toInt
        val flag = dut.io.legalLqFlag.peek().litValue != 0
        val nls = 1 + rng.nextInt(3)
        if (doDispatch) {
          dispatch(dut, 0, robVal, false, nls, start, flag)
          if (!canceledNow) {
            for (k <- 0 until nls) known += ((slotOf(start, k, size), slotFlag(flag, start, k, size)))
            if (known.length > 256) known.remove(0, 128)
          }
        }
        for (lane <- 0 until ldW) {
          if (known.nonEmpty) {
            val (v, f) = known(wbIndex % known.length)
            wbIndex += 1
            writeback(dut, lane, v, f, mmio = (wbIndex & 1) == 0, paddr = 0x400 + wbIndex)
          }
        }
        check(dut, "random-before", cycle)
        dut.clock.step(1)
        check(dut, "random-after", cycle)
      }
      assert(redirects > 0, "random driver did not issue redirects")
      assert(canceled > 0, "random driver did not issue canceled dispatch")
    }
  }

  it should "cover non-power-of-two 72 wrap, rollback and redirects" in {
    implicit val p: Parameters = params(72)
    val size = 72
    val enqW = p(XSCoreParamsKey).RenameWidth
    val ldW = p(XSCoreParamsKey).LoadPipelineWidth
    val vecW = p(XSCoreParamsKey).VecLoadPipelineWidth

    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      resetDut(dut, enqW, ldW, vecW)
      val rng = new Random(0x72)
      val known = ArrayBuffer.empty[(Int, Boolean)]
      var wbIndex = 0
      var pause = 0
      var wraps = 0
      var redirects = 0
      var canceled = 0
      for (cycle <- 0 until 1200) {
        if (pause > 0) pause -= 1
        idleReq(dut, enqW); idleLdin(dut, ldW); idleVec(dut, vecW); idleRedirect(dut)
        val can = dut.io.canAccept.peek().litValue != 0
        val doRedirect = pause == 0 && rng.nextInt(23) == 0
        var doDispatch = can && pause == 0 && rng.nextInt(3) != 0
        var robVal = rng.nextInt(16)
        var canceledNow = false
        if (doRedirect) {
          val redirVal = rng.nextInt(8)
          dut.io.redirectValid.poke(true.B)
          dut.io.redirectRobVal.poke(redirVal.U)
          dut.io.redirectRobFlag.poke(false.B)
          pause = 3
          redirects += 1
          if (can) {
            doDispatch = true
            robVal = redirVal + 1
            canceledNow = true
            canceled += 1
          }
        }
        val start = dut.io.legalLqIdx.peek().litValue.toInt
        val flag = dut.io.legalLqFlag.peek().litValue != 0
        val nls = 1 + rng.nextInt(4)
        if (doDispatch) {
          dispatch(dut, 0, robVal, false, nls, start, flag)
          if (!canceledNow) {
            for (k <- 0 until nls) known += ((slotOf(start, k, size), slotFlag(flag, start, k, size)))
            if (known.length > 512) known.remove(0, 256)
          }
        }
        for (lane <- 0 until ldW) {
          if (known.nonEmpty) {
            val (v, f) = known(wbIndex % known.length)
            wbIndex += 1
            writeback(dut, lane, v, f, mmio = (wbIndex & 1) == 0, paddr = 0x800 + wbIndex)
          }
        }
        check(dut, "np2-before", cycle)
        dut.clock.step(1)
        check(dut, "np2-after", cycle)
        val newFlag = dut.io.legalLqFlag.peek().litValue != 0
        if (newFlag != flag) wraps += 1
      }
      assert(wraps > 0, "non-power-of-two run did not wrap the tail")
      assert(redirects > 0, "non-power-of-two run did not issue redirects")
      assert(canceled > 0, "non-power-of-two run did not issue canceled dispatch")

    }
  }
}
