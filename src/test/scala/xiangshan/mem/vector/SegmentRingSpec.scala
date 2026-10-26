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
import chisel3.util._
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.backend.Bundles.DynInst

/** Verbatim baseline segment storage: `CircularQueuePtr` cursors plus separate
  * `data` / `stride` / `uopq` register arrays, with the baseline enqueue at
  * `enqPtr.value`, the data-only merge at an explicit address, and dequeue at
  * `deqPtr.value`. The legacy `allocated` register was dead and is omitted, and
  * the baseline exposes no `full` observable.
  */
class LegacySegmentRing(maxSize: Int, numRead: Int)(implicit p: Parameters) extends VLSUModule
  with HasCircularQueuePtrHelper {
  val io = IO(new SegmentRingIO(maxSize, numRead))

  class VSegUPtr extends CircularQueuePtr[VSegUPtr](maxSize)

  val data   = Reg(Vec(maxSize, UInt(VLEN.W)))
  val stride = Reg(Vec(maxSize, UInt(VLEN.W)))
  val uopq   = Reg(Vec(maxSize, new DynInst))
  val enqPtr = RegInit(0.U.asTypeOf(new VSegUPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VSegUPtr))

  when(io.enq.valid) {
    data(enqPtr.value)   := io.enq.bits.data
    stride(enqPtr.value) := io.enq.bits.stride
    uopq(enqPtr.value)   := io.enq.bits.uop
    enqPtr := enqPtr + 1.U
  }
  when(io.merge.valid) {
    data(io.merge.bits.addr) := io.merge.bits.data
  }

  io.enqPtr := enqPtr
  io.deqPtr := deqPtr
  io.empty  := isEmpty(enqPtr, deqPtr)

  io.deq.valid      := !isEmpty(enqPtr, deqPtr)
  io.deq.bits.data   := data(deqPtr.value)
  io.deq.bits.stride := stride(deqPtr.value)
  io.deq.bits.uop    := uopq(deqPtr.value)
  when(io.deq.fire) { deqPtr := deqPtr + 1.U }

  for (i <- 0 until numRead) {
    io.read(i).data.data   := data(io.read(i).addr)
    io.read(i).data.stride := stride(io.read(i).addr)
    io.read(i).data.uop    := uopq(io.read(i).addr)
  }
}

/** PRNG-driven miter of [[SegmentRing]] against the verbatim baseline ring.
  *
  * Per-slot per-field definedness tracks the unreset payload registers:
  * `uop`/`stride` become defined on enqueue, `data` on enqueue or merge, and
  * the tracking registers are unreset so they survive a mid-run reset exactly
  * like the payload. A one-time `initDefined` pulse clears the tracking at the
  * very start (initial-only explicit init). Only defined fields are compared;
  * undefined slots are masked so no simulator zero is assumed.
  */
class SegmentRingMiter(maxSize: Int, numRead: Int)(implicit p: Parameters) extends VLSUModule {
  val uopW = (new DynInst).getWidth

  val io = IO(new Bundle {
    val initDefined = Input(Bool())
    val refDeqValid = Output(Bool()); val newDeqValid = Output(Bool())
    val refDeqData  = Output(UInt(VLEN.W)); val newDeqData = Output(UInt(VLEN.W))
    val refDeqStr   = Output(UInt(VLEN.W)); val newDeqStr = Output(UInt(VLEN.W))
    val refDeqUop   = Output(UInt(uopW.W)); val newDeqUop = Output(UInt(uopW.W))
    val refReadData = Output(Vec(numRead, UInt(VLEN.W))); val newReadData = Output(Vec(numRead, UInt(VLEN.W)))
    val refReadStr  = Output(Vec(numRead, UInt(VLEN.W))); val newReadStr = Output(Vec(numRead, UInt(VLEN.W)))
    val refReadUop  = Output(Vec(numRead, UInt(uopW.W))); val newReadUop = Output(Vec(numRead, UInt(uopW.W)))
    val refEnqFlag  = Output(Bool()); val newEnqFlag = Output(Bool())
    val refEnqVal   = Output(UInt(log2Ceil(maxSize).W)); val newEnqVal = Output(UInt(log2Ceil(maxSize).W))
    val refDeqFlag  = Output(Bool()); val newDeqFlag = Output(Bool())
    val refDeqVal   = Output(UInt(log2Ceil(maxSize).W)); val newDeqVal = Output(UInt(log2Ceil(maxSize).W))
    val refEmpty    = Output(Bool()); val newEmpty = Output(Bool())
  })

  val ref = Module(new LegacySegmentRing(maxSize, numRead))
  val dut = Module(new SegmentRing(maxSize, numRead))

  val seeds = Seq.tabulate(64) { i =>
    (((BigInt(i) + 1) * BigInt("9e3779b97f4a7c15", 16)) + BigInt("123456789abcdef", 16)) &
      ((BigInt(1) << 64) - 1)
  }
  val lfsr = RegInit(VecInit(seeds.map(_.U(64.W))))
  for (i <- 0 until 64) {
    lfsr(i) := Cat(lfsr(i)(62, 0), lfsr(i)(63) ^ lfsr(i)(62) ^ lfsr(i)(61) ^ lfsr(i)(60)) ^ lfsr((i + 5) % 64)
  }
  val rng = Cat(lfsr.reverse)
  var cursor = 0
  def rnd[T <: Data](g: T): T = {
    val w = g.getWidth
    require(cursor + w <= rng.getWidth, s"PRNG exhausted at $cursor + $w > ${rng.getWidth}")
    val r = rng(cursor + w - 1, cursor).asTypeOf(g)
    cursor += w
    r
  }

  val enqValid   = rnd(Bool())
  val enqData    = rnd(UInt(VLEN.W))
  val enqStride  = rnd(UInt(VLEN.W))
  val enqUop     = rnd(new DynInst)
  val mergeValid = rnd(Bool())
  val mergeAddr  = (rnd(UInt(8.W)) % maxSize.U)(log2Ceil(maxSize) - 1, 0)
  val mergeData  = rnd(UInt(VLEN.W))
  val deqReady   = rnd(Bool())
  val readAddr   = VecInit((0 until numRead).map(_ => (rnd(UInt(8.W)) % maxSize.U)(log2Ceil(maxSize) - 1, 0)))

  def drive(m: SegmentRingIO): Unit = {
    m.enq.valid        := enqValid
    m.enq.bits.data    := enqData
    m.enq.bits.stride  := enqStride
    m.enq.bits.uop     := enqUop
    m.merge.valid      := mergeValid
    m.merge.bits.addr  := mergeAddr
    m.merge.bits.data  := mergeData
    m.deq.ready        := deqReady
    for (i <- 0 until numRead) { m.read(i).addr := readAddr(i) }
  }
  drive(ref.io)
  drive(dut.io)

  // Per-slot per-field definedness, unreset and preserved across mid-run reset.
  val uopDef  = Reg(Vec(maxSize, Bool()))
  val strDef  = Reg(Vec(maxSize, Bool()))
  val dataDef = Reg(Vec(maxSize, Bool()))
  when(io.initDefined) {
    for (i <- 0 until maxSize) { uopDef(i) := false.B; strDef(i) := false.B; dataDef(i) := false.B }
  }
  when(enqValid) {
    uopDef(ref.io.enqPtr.value)  := true.B
    strDef(ref.io.enqPtr.value)  := true.B
    dataDef(ref.io.enqPtr.value) := true.B
  }
  when(mergeValid) {
    dataDef(mergeAddr) := true.B
  }

  val deqAddr = ref.io.deqPtr.value
  io.refDeqValid := ref.io.deq.valid
  io.newDeqValid := dut.io.deq.valid
  io.refDeqData  := Mux(dataDef(deqAddr), ref.io.deq.bits.data, 0.U)
  io.newDeqData  := Mux(dataDef(deqAddr), dut.io.deq.bits.data, 0.U)
  io.refDeqStr   := Mux(strDef(deqAddr), ref.io.deq.bits.stride, 0.U)
  io.newDeqStr   := Mux(strDef(deqAddr), dut.io.deq.bits.stride, 0.U)
  io.refDeqUop   := Mux(uopDef(deqAddr), ref.io.deq.bits.uop.asUInt, 0.U)
  io.newDeqUop   := Mux(uopDef(deqAddr), dut.io.deq.bits.uop.asUInt, 0.U)
  for (i <- 0 until numRead) {
    io.refReadData(i) := Mux(dataDef(readAddr(i)), ref.io.read(i).data.data, 0.U)
    io.newReadData(i) := Mux(dataDef(readAddr(i)), dut.io.read(i).data.data, 0.U)
    io.refReadStr(i)  := Mux(strDef(readAddr(i)), ref.io.read(i).data.stride, 0.U)
    io.newReadStr(i)  := Mux(strDef(readAddr(i)), dut.io.read(i).data.stride, 0.U)
    io.refReadUop(i)  := Mux(uopDef(readAddr(i)), ref.io.read(i).data.uop.asUInt, 0.U)
    io.newReadUop(i)  := Mux(uopDef(readAddr(i)), dut.io.read(i).data.uop.asUInt, 0.U)
  }
  io.refEnqFlag := ref.io.enqPtr.flag; io.newEnqFlag := dut.io.enqPtr.flag
  io.refEnqVal  := ref.io.enqPtr.value; io.newEnqVal := dut.io.enqPtr.value
  io.refDeqFlag := ref.io.deqPtr.flag; io.newDeqFlag := dut.io.deqPtr.flag
  io.refDeqVal  := ref.io.deqPtr.value; io.newDeqVal := dut.io.deqPtr.value
  io.refEmpty   := ref.io.empty; io.newEmpty := dut.io.empty
}

class SegmentRingSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.CRITICAL, perfDBHartID = 0
    )
  }

  private def compare(dut: SegmentRingMiter, n: Int, cycle: Int, tag: String): Unit = {
    assert(dut.io.refDeqValid.peek().litValue == dut.io.newDeqValid.peek().litValue, s"$tag deq.valid at $cycle")
    assert(dut.io.refDeqData.peek().litValue  == dut.io.newDeqData.peek().litValue,  s"$tag deq.data at $cycle")
    assert(dut.io.refDeqStr.peek().litValue   == dut.io.newDeqStr.peek().litValue,   s"$tag deq.stride at $cycle")
    assert(dut.io.refDeqUop.peek().litValue   == dut.io.newDeqUop.peek().litValue,   s"$tag deq.uop at $cycle")
    for (i <- 0 until n) {
      assert(dut.io.refReadData(i).peek().litValue == dut.io.newReadData(i).peek().litValue, s"$tag read.data($i) at $cycle")
      assert(dut.io.refReadStr(i).peek().litValue  == dut.io.newReadStr(i).peek().litValue,  s"$tag read.stride($i) at $cycle")
      assert(dut.io.refReadUop(i).peek().litValue  == dut.io.newReadUop(i).peek().litValue,  s"$tag read.uop($i) at $cycle")
    }
    assert(dut.io.refEnqFlag.peek().litValue == dut.io.newEnqFlag.peek().litValue, s"$tag enqPtr.flag at $cycle")
    assert(dut.io.refEnqVal.peek().litValue  == dut.io.newEnqVal.peek().litValue,  s"$tag enqPtr.value at $cycle")
    assert(dut.io.refDeqFlag.peek().litValue == dut.io.newDeqFlag.peek().litValue, s"$tag deqPtr.flag at $cycle")
    assert(dut.io.refDeqVal.peek().litValue  == dut.io.newDeqVal.peek().litValue,  s"$tag deqPtr.value at $cycle")
    assert(dut.io.refEmpty.peek().litValue == dut.io.newEmpty.peek().litValue, s"$tag empty at $cycle")
  }

  behavior of "SegmentRing"

  for (capacity <- Seq(6, 8)) {
    it should s"match the baseline ring at capacity $capacity through wrap, simultaneous retire/admit, reset-concurrent writes and holds" in {
      test(new SegmentRingMiter(capacity, 3)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        var cycle = 0
        def edge(tag: String): Unit = { dut.clock.step(1); compare(dut, 3, cycle, tag); cycle += 1 }

        // Initial-only explicit init while the PRNG already drives enqueue/merge
        // (reset-concurrent payload writes on the unreset registers).
        dut.io.initDefined.poke(true.B)
        dut.reset.poke(true.B)
        for (_ <- 0 until 4) { edge("reset-concurrent") }
        dut.io.initDefined.poke(false.B)
        dut.reset.poke(false.B)

        for (_ <- 0 until 700) { edge("run") }

        // Nonzero mid-run reset; definedness and payloads persist.
        dut.reset.poke(true.B)
        for (_ <- 0 until 4) { edge("midreset") }
        dut.reset.poke(false.B)

        for (_ <- 0 until 400) { edge("postreset") }
      }
      ()
    }
  }
}
