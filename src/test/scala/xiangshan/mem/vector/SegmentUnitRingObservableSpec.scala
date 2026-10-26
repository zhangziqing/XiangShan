/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._
import chisel3.reflect.DataMirror
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._

/** Test-only probe exposing the real `VSegmentUnit` ring cursors and the
  * baseline `isAfter(enqPtr, deqPtr)` observable, so the caller integration is
  * observable (not just a helper unit test).
  */
class VSegmentUnitRingProbe(implicit p: Parameters) extends VSegmentUnit {
  val dbgEnqPtr  = IO(Output(new SegmentRingPtr(VSegmentBufferSize)))
  val dbgDeqPtr  = IO(Output(new SegmentRingPtr(VSegmentBufferSize)))
  val dbgEmpty   = IO(Output(Bool()))
  val dbgIsAfter = IO(Output(Bool()))
  dbgEnqPtr  := segQueue.io.enqPtr
  dbgDeqPtr  := segQueue.io.deqPtr
  dbgEmpty   := segQueue.io.empty
  dbgIsAfter := isAfter(segQueue.io.enqPtr, segQueue.io.deqPtr)
}

/** Caller observable test on the real `VSegmentUnit`:
  *  - every input leaf is driven to a defined value before reset (no simulator
  *    default);
  *  - `io.in.ready` is always true (whole-caller ready semantics, no invented
  *    ring backpressure and no invented `full`);
  *  - eight directed offers take the value cursor 0..7 then back to 0 and flip
  *    the wrap flag exactly as the baseline `CircularQueuePtr` arithmetic does,
  *    with `isAfter(enqPtr, deqPtr)` becoming true at full wrap;
  *  - cursors and `empty` hold when the offers stop.
  */
class SegmentUnitRingObservableSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.CRITICAL, perfDBHartID = 0
    )
  }.alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug,
      // arbitrary directed enqueue traffic: suppress design-premise checks
      true,
      here(DebugOptionsKey).EnableXMR
    )
  })

  /** Recursively drive every input leaf to a defined value before reset. */
  private def initInputs(io: Data): Unit = {
    def rec(d: Data): Unit = d match {
      case v: Vec[_] => v.foreach(rec)
      case r: Record => r.elements.foreach { case (_, e) => rec(e) }
      case _ =>
        if (DataMirror.directionOf(d) == ActualDirection.Input) {
          d match {
            case b: Bool => b.poke(false.B)
            case u: UInt => u.poke(0.U)
            case s: SInt => s.poke(0.S)
            case _       => ()
          }
        }
        ()
    }
    rec(io)
    ()
  }

  behavior of "VSegmentUnit SegmentRing caller"

  it should "accept eight offers through the always-ready caller and wrap the enqueue cursor" in {
    test(new VSegmentUnitRingProbe).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initInputs(dut.io)
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)

      // Whole-caller ready semantics and empty initial state.
      dut.io.in.ready.expect(true.B)
      dut.dbgEmpty.expect(true.B)
      dut.dbgIsAfter.expect(false.B)
      dut.dbgEnqPtr.value.expect(0.U)
      dut.dbgEnqPtr.flag.expect(false.B)
      dut.dbgDeqPtr.value.expect(0.U)
      dut.dbgDeqPtr.flag.expect(false.B)

      // Eight directed offers: valid alone is enough because ready is always
      // true. The value cursor steps 0..7 and, on the eighth edge, returns to 0
      // with the wrap flag set (baseline `CircularQueuePtr` full wrap).
      dut.io.in.valid.poke(true.B)
      for (cycle <- 0 until 8) {
        dut.io.in.ready.expect(true.B)
        dut.clock.step(1)
      }
      dut.io.in.valid.poke(false.B)
      dut.dbgEnqPtr.value.expect(0.U)
      dut.dbgEnqPtr.flag.expect(true.B)
      dut.dbgDeqPtr.value.expect(0.U)
      dut.dbgDeqPtr.flag.expect(false.B)
      dut.dbgEmpty.expect(false.B)
      dut.dbgIsAfter.expect(true.B)

      // Hold: no offers, the ring state stays put.
      dut.clock.step(1)
      dut.dbgEnqPtr.value.expect(0.U)
      dut.dbgEnqPtr.flag.expect(true.B)
      dut.dbgDeqPtr.value.expect(0.U)
      dut.dbgEmpty.expect(false.B)
      dut.dbgIsAfter.expect(true.B)
    }
    ()
  }
}
