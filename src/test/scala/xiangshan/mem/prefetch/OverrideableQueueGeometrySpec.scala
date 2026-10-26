// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import _root_.utils.QueueModel.{OverrideableQueue => ModelQueue, QueueRTL}
import _root_.utils.{OverrideableQueue => LegacyQueue}
import scala.util.Random

class ObservableLegacyOverwriteQueue(size: Int) extends LegacyQueue(UInt(16.W), size) {
  val unusedReadPosition = IO(Output(Bool()))
  unusedReadPosition := rd_ptr >= size.U
}

/** Compare the original binary-index policy, including unused positions. */
class OverwriteGeometryMiter(size: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Valid(UInt(16.W)))
    val ready = Input(Bool())
    val currentValid = Output(Bool())
    val legacyValid = Output(Bool())
    val currentBits = Output(UInt(16.W))
    val legacyBits = Output(UInt(16.W))
    val unusedReadPosition = Output(Bool())
  })
  val current = Module(new ModelQueue(UInt(16.W), size)(p, QueueRTL))
  val legacy = Module(new ObservableLegacyOverwriteQueue(size))
  current.io.in := io.in
  legacy.io.in := io.in
  current.io.out.ready := io.ready
  legacy.io.out.ready := io.ready
  io.currentValid := current.io.out.valid
  io.legacyValid := legacy.io.out.valid
  io.currentBits := current.io.out.bits
  io.legacyBits := legacy.io.out.bits
  io.unusedReadPosition := legacy.unusedReadPosition
}

class OverrideableQueueGeometrySpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty

  for (size <- Seq(1, 3, 4, 6)) {
    it should s"preserve baseline binary-index overwrite behavior at capacity $size" in {
      test(new OverwriteGeometryMiter(size))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          val random = new Random(0x504854L + size)
          dut.io.in.valid.poke(false.B)
          dut.io.in.bits.poke(0.U)
          dut.io.ready.poke(false.B)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)

          def compare(): Unit = {
            dut.io.currentValid.expect(dut.io.legacyValid.peek())
            // Valid data and defined unused-index behavior (singleton included).
            if (dut.io.legacyValid.peek().litToBoolean ||
                dut.io.unusedReadPosition.peek().litToBoolean) {
              dut.io.currentBits.expect(dut.io.legacyBits.peek())
            }
          }
          for (cycle <- 0 until 300) {
            // Fill past capacity with a stalled reader, drain, then interleave.
            dut.io.in.valid.poke((cycle < size + 3 ||
              (cycle >= 2 * size + 8 && random.nextBoolean())).B)
            dut.io.ready.poke((cycle >= size + 3 && random.nextBoolean()).B)
            dut.io.in.bits.poke(random.nextInt(65536).U)
            compare()
            dut.clock.step()
            compare()
            if (cycle == 149) {
              dut.reset.poke(true.B)
              dut.clock.step(2)
              dut.reset.poke(false.B)
              compare()
            }
          }
        }
    }
  }
}
