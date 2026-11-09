// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import chisel3._
import chisel3.util._
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.backend.fu.FuType

/** Compare both owners under the same explicit asynchronous reset. */
class VLoadQueueResetHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val queueReset = Input(Bool())
    val allocate = Input(Bool())
    val rob = Input(UInt(8.W))
    val uop = Input(UInt(7.W))
    val currentState = Output(new VLoadQueueDbg)
    val referenceState = Output(new VLoadQueueDbg)
  })
  private val current = withReset(io.queueReset.asAsyncReset) {
    Module(new TestableVirtualLoadQueue)
  }
  private val reference = withReset(io.queueReset.asAsyncReset) {
    Module(new TestableLegacyVirtualLoadQueue)
  }
  dontTouch(current.io)
  Seq(current.io.redirect, reference.io.redirect).foreach(r => r := 0.U.asTypeOf(r))
  Seq(current.io.vecCommit, reference.io.vecCommit).foreach(_.foreach(c => c := 0.U.asTypeOf(c)))
  Seq(current.io.enq, reference.io.enq).foreach { enq =>
    enq.sqCanAccept := true.B
    enq.needAlloc.foreach(_ := false.B)
    enq.req.foreach(r => r := 0.U.asTypeOf(r))
    enq.needAlloc(0) := io.allocate
    enq.req(0).valid := io.allocate
    enq.req(0).bits.fuType := FuType.vldu.U
    enq.req(0).bits.robIdx.value := io.rob
    enq.req(0).bits.uopIdx := io.uop
    enq.req(0).bits.lqIdx := enq.resp(0)
    enq.req(0).bits.numLsElem := 1.U
  }
  Seq(current.io.ldin, reference.io.ldin).foreach(_.foreach { lane =>
    lane.valid := false.B
    lane.bits := 0.U.asTypeOf(lane.bits)
  })
  io.currentState := current.dbg
  io.referenceState := reference.dbg
}

class VirtualLoadQueueResetSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head.copy(VirtualLoadQueueSize = 8)
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => core
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false,
      perfLevel = XSPerfLevel.CRITICAL, perfDBHartID = 0)
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug, here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform, here(DebugOptionsKey).EnableXMR)
  })

  it should "match baseline asynchronous metadata reset and unreset payload writes" in {
    test(new VLoadQueueResetHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      def compareMetadata(): Unit = {
        for (slot <- 0 until core.VirtualLoadQueueSize) {
          dut.io.currentState.isvec(slot).expect(dut.io.referenceState.isvec(slot).peek())
          dut.io.currentState.allocated(slot).expect(dut.io.referenceState.allocated(slot).peek())
        }
      }
      def offer(rob: Int, uop: Int): Unit = {
        dut.io.allocate.poke(true.B)
        dut.io.rob.poke(rob.U)
        dut.io.uop.poke(uop.U)
        dut.clock.step()
        dut.io.allocate.poke(false.B)
        compareMetadata()
      }
      dut.io.allocate.poke(false.B); dut.io.rob.poke(0.U); dut.io.uop.poke(0.U)
      dut.io.queueReset.poke(true.B); dut.clock.step(2); dut.io.queueReset.poke(false.B)
      compareMetadata()
      offer(42, 3)
      dut.io.currentState.isvec(0).expect(true.B)
      dut.io.currentState.allocated(0).expect(true.B)

      // Assertion and deassertion entirely between clock edges must clear both
      // reset fields, while leaving the already defined rob/uop payload alone.
      dut.io.queueReset.poke(true.B)
      dut.io.referenceState.isvec(0).expect(false.B)
      compareMetadata()
      dut.io.currentState.robIdx(0).value.expect(42.U)
      dut.io.queueReset.poke(false.B)
      compareMetadata()
      dut.clock.step()
      compareMetadata()
      dut.io.currentState.isvec(0).expect(false.B)

      offer(51, 9)
      dut.io.currentState.isvec(0).expect(true.B)
      // A held reset clears metadata immediately; a concurrent allocation edge
      // still writes the fields that had no reset in the baseline.
      dut.io.queueReset.poke(true.B)
      compareMetadata()
      offer(85, 45)
      dut.io.currentState.isvec(0).expect(false.B)
      dut.io.currentState.allocated(0).expect(false.B)
      dut.io.referenceState.robIdx(0).value.expect(85.U)
      dut.io.referenceState.uopIdx(0).expect(45.U)
      dut.io.currentState.robIdx(0).value.expect(dut.io.referenceState.robIdx(0).value.peek())
      dut.io.currentState.uopIdx(0).expect(dut.io.referenceState.uopIdx(0).peek())
      dut.io.queueReset.poke(false.B)
      compareMetadata()
      offer(91, 7)
      dut.io.currentState.allocated(0).expect(true.B)
    }
  }
}
