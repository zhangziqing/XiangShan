// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.backend.fu.FuType

class VLoadQueueContentSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => base(XSTileKey).head.copy(VirtualLoadQueueSize = 8)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.CRITICAL, 0)
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug, here(DebugOptionsKey).FPGAPlatform,
      here(DebugOptionsKey).EnableXMR)
  })

  private def zeroInputs(data: Data): Unit = data match {
    case v: Vec[_] => v.foreach(zeroInputs)
    case r: Record => r.elements.values.foreach(zeroInputs)
    case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
    case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
    case _ =>
  }

  it should "preserve old-state vector matching, multilane allocation and later writeback" in {
    test(new VLoadQueueMiterHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      zeroInputs(dut.io)
      dut.io.sqCanAccept.poke(true.B)
      dut.io.initDefined.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(3); dut.reset.poke(false.B)
      dut.io.initDefined.poke(false.B)
      def equal(): Unit = {
        dut.io.mismatch.expect(false.B)
        assert(dut.io.newOut.peek().litValue == dut.io.refOut.peek().litValue)
      }
      def edge(): Unit = { equal(); dut.clock.step(); equal() }
      def slot(i: Int, allocated: Boolean, committed: Boolean, vector: Boolean): Unit = {
        dut.io.dbgIdx.poke(i.U)
        dut.io.dbgAllocated.expect(allocated.B)
        dut.io.dbgCommitted.expect(committed.B)
        dut.io.dbgIsvec.expect(vector.B)
      }
      def request(lane: Int, index: Int, flow: Int, rob: Int, vector: Boolean): Unit = {
        dut.io.needAlloc(lane).poke(true.B); dut.io.reqValid(lane).poke(true.B)
        dut.io.reqLqVal(lane).poke(index.U); dut.io.reqNlsElem(lane).poke(flow.U)
        dut.io.reqRobVal(lane).poke(rob.U); dut.io.reqUopIdx(lane).poke(1.U)
        dut.io.reqFuType(lane).poke((if (vector) FuType.vldu.ohid else FuType.ldu.ohid).U)
      }
      def feedback(rob: Int): Unit = {
        dut.io.vcValid(0).poke(true.B); dut.io.vcRobVal(0).poke(rob.U)
        dut.io.vcUopIdx(0).poke(1.U)
      }
      def noRequests(): Unit = {
        for (j <- 0 until p(XSCoreParamsKey).RenameWidth) {
          dut.io.reqValid(j).poke(false.B); dut.io.needAlloc(j).poke(false.B)
        }
      }

      // Four flows from three dispatch ports. Feedback for the new vector
      // matches its payload but must observe the still-invalid old slots.
      request(0, 0, 1, 10, false)
      request(1, 1, 2, 11, true)
      request(2, 3, 1, 12, false)
      feedback(11)
      dut.io.ldinValid(0).poke(true.B); dut.io.ldinUpdOk(0).poke(true.B)
      dut.io.ldinLqVal(0).poke(0.U); dut.io.ldinMmio(0).poke(true.B)
      dut.io.ldinPaddr(0).poke(0x123.U)
      edge()
      slot(0, true, true, false)
      dut.io.dbgMmio.expect(true.B); dut.io.dbgPaddr.expect(0x123.U)
      slot(1, true, false, true); slot(2, true, false, true)
      slot(3, true, false, false)

      // Matching old vector entries update together; the scalar neighbor is
      // committed by the independently indexed later writeback operation.
      noRequests()
      dut.io.ldinLqVal(0).poke(3.U)
      edge()
      slot(1, true, true, true); slot(2, true, true, true)
      slot(3, true, true, false)
      dut.io.ldinValid(0).poke(false.B); dut.io.vcValid(0).poke(false.B)
      for (_ <- 0 until 6) edge()
      for (i <- 0 until 4) { dut.io.dbgIdx.poke(i.U); dut.io.dbgAllocated.expect(false.B) }

      // Content matching and cancellation both read old state on this edge.
      val start = dut.io.legalLqIdx.peek().litValue.toInt
      val flag = dut.io.legalLqFlag.peek().litValue != 0
      request(0, start, 2, 20, true); dut.io.reqLqFlag(0).poke(flag.B)
      edge(); noRequests()
      feedback(20)
      dut.io.redirectValid.poke(true.B); dut.io.redirectRobVal.poke(19.U)
      edge()
      for (i <- start until start + 2) slot(i % 8, false, true, true)
      dut.io.redirectValid.poke(false.B); dut.io.vcValid(0).poke(false.B)
      for (_ <- 0 until 4) edge()
    }
  }
}
