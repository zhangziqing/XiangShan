// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class ReservationEnqueueHarness(n: Int)(implicit p: Parameters)
  extends RingQueue[UInt](UInt(8.W), n, 2, 1)(p, QueueRTL) {
  val io = IO(new Bundle {
    val valid = Input(Vec(2, Bool()))
    val admit = Input(Vec(2, Bool()))
    val data = Input(Vec(2, UInt(8.W)))
    val start = Input(Vec(2, UInt(log2Ceil(n).W)))
    val phase = Input(Vec(2, Bool()))
    val length = Input(Vec(2, UInt(log2Ceil(n + 1).W)))
    val reserve = Input(UInt(log2Ceil(n + 1).W))
    val consume = Input(UInt(log2Ceil(n + 1).W))
    val rollback = Input(Bool())
    val target = Input(UInt(log2Ceil(n).W))
    val targetPhase = Input(Bool())
    val clear = Input(Bool())
    val patch = Input(Bool())
    val values = Output(Vec(n, UInt(8.W)))
    val head = Output(UInt(log2Ceil(n).W))
    val tail = Output(UInt(log2Ceil(n).W))
    val tailPhase = Output(Bool())
    val taken = Output(Vec(2, Bool()))
  })
  override protected def initContents: Option[Seq[UInt]] = Some(Seq.fill(entries)(0.U(8.W)))
  override protected def enqGuard(elements: Seq[ValidIO[UInt]]): Seq[Bool] =
    elements.zip(io.admit).map { case (e, a) => e.valid && a }
  flush := io.clear
  recover := io.rollback
  recoverPtr.value := io.target
  recoverPtr.flag := io.targetPhase
  val elements = (0 until 2).map { i =>
    val e = Wire(Valid(UInt(8.W)))
    e.valid := io.valid(i)
    e.bits := io.data(i)
    e
  }
  val ranges = (0 until 2).map { i =>
    val start = Wire(new QueuePtr)
    start.value := io.start(i)
    start.flag := io.phase(i)
    (start, io.length(i))
  }
  io.taken := VecInit(enq(elements, ranges, io.reserve))
  deq(io.consume)
  update { e => when(io.patch && e === 17.U) { e := 99.U } }
  for (i <- 0 until n) io.values(i) := read(i)
  io.head := deqPtr.value
  io.tail := enqPtr.value
  io.tailPhase := enqPtr.flag
}

class ReservationEnqueueSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty
  for (n <- Seq(1, 6, 8, 72)) {
    it should s"preserve independent reservations, overlap and old-state updates at depth $n" in {
      test(new ReservationEnqueueHarness(n)).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
        def idle(): Unit = {
          for (i <- 0 until 2) {
            d.io.valid(i).poke(false.B); d.io.admit(i).poke(true.B)
            d.io.data(i).poke(0.U); d.io.start(i).poke(0.U)
            d.io.phase(i).poke(false.B); d.io.length(i).poke(0.U)
          }
          d.io.reserve.poke(0.U); d.io.consume.poke(0.U)
          d.io.rollback.poke(false.B); d.io.target.poke(0.U)
          d.io.targetPhase.poke(false.B); d.io.clear.poke(false.B); d.io.patch.poke(false.B)
        }
        idle(); d.reset.poke(true.B); d.clock.step(); d.reset.poke(false.B)
        // No payload accepted, but all n positions are reserved.
        d.io.reserve.poke(n.U); d.clock.step(); idle()
        d.io.tail.expect(0.U); d.io.tailPhase.expect(true.B)
        for (i <- 0 until n) d.io.values(i).expect(0.U)
        // Both absolute ranges wrap. Port 0 wins; update must read pre-edge zero.
        for (i <- 0 until 2) {
          d.io.valid(i).poke(true.B); d.io.start(i).poke((n - 1).U)
          d.io.length(i).poke(math.min(2, n).U)
          d.io.data(i).poke((17 + i).U)
        }
        d.io.patch.poke(true.B); d.io.rollback.poke(true.B)
        d.io.target.poke((n - 1).U); d.io.targetPhase.poke(false.B)
        d.io.reserve.poke(1.U); d.clock.step(); idle()
        d.io.tail.expect((n - 1).U); d.io.tailPhase.expect(false.B)
        d.io.values(n - 1).expect(17.U); d.io.values(0).expect(17.U)
        // A later update wins over enqueue only where the old entry matches.
        d.io.patch.poke(true.B); d.io.valid(0).poke(true.B)
        d.io.length(0).poke(n.U); d.io.data(0).poke(31.U)
        d.clock.step(); idle()
        for (i <- 0 until n) d.io.values(i).expect((if (i == 0 || i == n - 1) 99 else 31).U)
        // Rejected ports never initialize payload; flush/reset own endpoints.
        d.io.valid(0).poke(true.B); d.io.admit(0).poke(false.B)
        d.io.length(0).poke(n.U); d.io.data(0).poke(55.U); d.clock.step()
        d.io.values(0).expect(99.U)
        d.io.admit(0).poke(true.B); d.io.clear.poke(true.B); d.clock.step()
        d.io.tail.expect(0.U); d.io.head.expect(0.U); d.io.values(0).expect(99.U)
        d.io.clear.poke(false.B); d.reset.poke(true.B); d.clock.step()
        for (i <- 0 until n) d.io.values(i).expect(0.U)
      }
    }
  }
}
