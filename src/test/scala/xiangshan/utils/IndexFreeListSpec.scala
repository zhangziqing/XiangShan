// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.utils

import chisel3._
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.mem.FreeList
import _root_.utils.QueueModel.{IndexFreeList, QueueRTL}
import scala.util.Random

/** Miter harness: `IndexFreeList` versus the legacy `xiangshan.mem.FreeList`. */
class IflHarness(size: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean)
    (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val allocateReq = Input(Vec(allocWidth, Bool()))
    val doAllocate  = Input(Vec(allocWidth, Bool()))
    val free        = Input(UInt(size.W))
    val newSlot     = Output(Vec(allocWidth, UInt()))
    val newCan      = Output(Vec(allocWidth, Bool()))
    val refSlot     = Output(Vec(allocWidth, UInt()))
    val refCan      = Output(Vec(allocWidth, Bool()))
    val newCount    = Output(UInt())
    val refCount    = Output(UInt())
    val newEmpty    = Output(Bool())
    val refEmpty    = Output(Bool())
    val newPerf     = Output(UInt(6.W))
    val refPerf     = Output(UInt(6.W))
  })

  val newFl = Module(new IndexFreeList(size, allocWidth, freeWidth, enablePreAlloc, "miterNew")(p, QueueRTL))
  val refFl = Module(new FreeList(size, allocWidth, freeWidth, enablePreAlloc, "miterRef"))

  newFl.io.allocateReq := io.allocateReq
  newFl.io.doAllocate  := io.doAllocate
  newFl.io.free        := io.free
  refFl.io.allocateReq := io.allocateReq
  refFl.io.doAllocate  := io.doAllocate
  refFl.io.free        := io.free

  io.newSlot  := newFl.io.allocateSlot
  io.newCan   := newFl.io.canAllocate
  io.refSlot  := refFl.io.allocateSlot
  io.refCan   := refFl.io.canAllocate
  io.newCount := newFl.io.validCount
  io.refCount := refFl.io.validCount
  io.newEmpty := newFl.io.empty
  io.refEmpty := refFl.io.empty
  io.newPerf := newFl.getPerf(0).value
  io.refPerf := refFl.getPerf(0).value
}

class IndexFreeListSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => core
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.CRITICAL,
      perfDBHartID = 0
    )
  })
  private val cp = p(XSCoreParamsKey)

  private def initInputs(dut: IflHarness, allocWidth: Int): Unit = {
    for (i <- 0 until allocWidth) {
      dut.io.allocateReq(i).poke(false.B)
      dut.io.doAllocate(i).poke(false.B)
    }
    dut.io.free.poke(0.U)
    ()
  }

  /** Four reset edges and one idle edge with every input held at zero, so the
    * registered pre-alloc `allocateSlot` (`RegEnable(freeList(deqPtr.value),
    * enablePreAlloc.B)`) is initialized before any comparison.
    */
  private def resetIdle(dut: IflHarness, allocWidth: Int): Unit = {
    initInputs(dut, allocWidth)
    dut.reset.poke(true.B)
    for (_ <- 0 until 4) { dut.clock.step(1) }
    dut.reset.poke(false.B)
    dut.clock.step(1)
    ()
  }

  private def compare(dut: IflHarness, allocWidth: Int, cycle: Int, tag: String): Unit = {
    for (i <- 0 until allocWidth) {
      assert(dut.io.newSlot(i).peek().litValue == dut.io.refSlot(i).peek().litValue,
        s"$tag allocateSlot($i) at $cycle")
      assert(dut.io.newCan(i).peek().litValue == dut.io.refCan(i).peek().litValue,
        s"$tag canAllocate($i) at $cycle")
    }
    assert(dut.io.newCount.peek().litValue == dut.io.refCount.peek().litValue, s"$tag validCount at $cycle")
    assert(dut.io.newEmpty.peek().litValue == dut.io.refEmpty.peek().litValue, s"$tag empty at $cycle")
    assert(dut.io.newPerf.peek().litValue == dut.io.refPerf.peek().litValue, s"$tag empty perf event at $cycle")
    ()
  }

  private def preEdgeCan(dut: IflHarness, allocWidth: Int): Seq[Int] =
    (0 until allocWidth).map(i => dut.io.refCan(i).peek().litValue.toInt)

  /** Unconstrained random miter: `free` is arbitrary and may repeat an
    * already-free index, so it asserts only `new == ref` (slot/can/count/empty),
    * never an absolute occupancy contract.
    */
  private def runUnconstrained(size: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean,
                               seed: Long, cycles: Int, tag: String): Unit = {
    test(new IflHarness(size, allocWidth, freeWidth, enablePreAlloc))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      resetIdle(dut, allocWidth)
      val rng = new Random(seed)
      for (cycle <- 0 until cycles) {
        for (i <- 0 until allocWidth) {
          dut.io.allocateReq(i).poke((rng.nextInt(3) != 0).B)
          dut.io.doAllocate(i).poke((rng.nextInt(3) != 0).B)
        }
        dut.io.free.poke(BigInt(size, rng).U(size.W))
        // Compare after every edge, including the final one.
        dut.clock.step(1)
        compare(dut, allocWidth, cycle, tag)
      }
    }
    ()
  }

  behavior of "IndexFreeList"

  // Six actual caller constructors:
  //   VLMergeBufferImp  (VMergeBuffer.scala:425): VlMergeBufferSize, VecLoadPipelineWidth, VLUopWritebackWidth, false
  //   VSMergeBufferImp  (VMergeBuffer.scala:502): VsMergeBufferSize, VecStorePipelineWidth, VSUopWritebackWidth, false
  //   LoadQueueRAR      (LoadQueueRAR.scala:115):  LoadQueueRARSize,     LoadPipelineWidth,      4, false
  //   LoadQueueRAW      (LoadQueueRAW.scala:106):  LoadQueueRAWSize,     LoadPipelineWidth,      4, true
  //   LoadQueueUncache  (LoadQueueUncache.scala:302): LoadUncacheBufferSize, LoadPipelineWidth,  4, true
  //   LoadQueueReplay   (LoadQueueReplay.scala:250):  LoadQueueReplaySize,  LoadPipelineWidth,  4, true
  it should "match the VL merge-buffer caller freelist geometry" in {
    runUnconstrained(cp.VlMergeBufferSize, cp.VecLoadPipelineWidth, cp.VLUopWritebackWidth,
      enablePreAlloc = false, seed = 0x11L, cycles = 1500, "VL")
  }
  it should "match the VS merge-buffer caller freelist geometry" in {
    runUnconstrained(cp.VsMergeBufferSize, cp.VecStorePipelineWidth, cp.VSUopWritebackWidth,
      enablePreAlloc = false, seed = 0x22L, cycles = 1500, "VS")
  }
  it should "match the LoadQueueRAR caller freelist geometry" in {
    runUnconstrained(cp.LoadQueueRARSize, cp.LoadPipelineWidth, 4,
      enablePreAlloc = false, seed = 0x33L, cycles = 1500, "RAR")
  }
  it should "match the LoadQueueRAW caller freelist geometry" in {
    runUnconstrained(cp.LoadQueueRAWSize, cp.LoadPipelineWidth, 4,
      enablePreAlloc = true, seed = 0x44L, cycles = 1500, "RAW")
  }
  it should "match the LoadQueueUncache caller freelist geometry" in {
    runUnconstrained(cp.LoadUncacheBufferSize, cp.LoadPipelineWidth, 4,
      enablePreAlloc = true, seed = 0x55L, cycles = 1500, "Uncache")
  }
  it should "match the LoadQueueReplay caller freelist geometry" in {
    runUnconstrained(cp.LoadQueueReplaySize, cp.LoadPipelineWidth, 4,
      enablePreAlloc = true, seed = 0x66L, cycles = 1500, "Replay")
  }

  it should "exhaust through the legal prefix and reclaim with the free latency" in {
    val size = 8; val allocWidth = 2; val freeWidth = 4
    test(new IflHarness(size, allocWidth, freeWidth, enablePreAlloc = false))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      resetIdle(dut, allocWidth)

      // Allocate only the legal prefix: poke a full request prefix, sample the
      // pre-edge canAllocate, and gate doAllocate with it. `numAllocate` is
      // PopCount(doAllocate), so the accepted lanes are exactly the legal
      // prefix and the list never over-allocates.
      for (i <- 0 until allocWidth) { dut.io.allocateReq(i).poke(true.B) }
      var cycle = 0
      var empty = false
      while (!empty && cycle < 8) {
        val can = preEdgeCan(dut, allocWidth)
        for (i <- 0 until allocWidth) { dut.io.doAllocate(i).poke((can(i) == 1).B) }
        compare(dut, allocWidth, cycle, "exhaust-can")
        dut.clock.step(1)
        compare(dut, allocWidth, cycle, "exhaust")
        empty = dut.io.newEmpty.peek().litValue == 1
        cycle += 1
      }
      assert(empty, "legal-prefix allocation must exhaust the 8-entry list")
      assert(cycle <= 4, s"size-8 freelist must exhaust within 4 legal cycles, took $cycle")
      dut.io.newEmpty.expect(true.B)

      // Deassert the request and the accepts before reclaiming.
      for (i <- 0 until allocWidth) { dut.io.allocateReq(i).poke(false.B); dut.io.doAllocate(i).poke(false.B) }
      dut.clock.step(1)
      compare(dut, allocWidth, 100, "exhaust-hold")
      dut.io.newEmpty.expect(true.B)

      // Single-edge free pulse, then deassert. The free path is
      // freeMask -> selection -> tail, so empty is still true for two observed
      // edges and drops on the third.
      dut.io.free.poke(1.U)
      dut.clock.step(1); compare(dut, allocWidth, 101, "free1"); dut.io.newEmpty.expect(true.B)
      dut.io.free.poke(0.U)
      dut.clock.step(1); compare(dut, allocWidth, 102, "free2"); dut.io.newEmpty.expect(true.B)
      dut.clock.step(1); compare(dut, allocWidth, 103, "free3"); dut.io.newEmpty.expect(false.B)
    }
    ()
  }

  it should "stress legal allocate/free against an allocated-set oracle" in {
    val size = 8; val allocWidth = 2; val freeWidth = 4
    test(new IflHarness(size, allocWidth, freeWidth, enablePreAlloc = false))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      resetIdle(dut, allocWidth)
      val rng = new Random(0x77L)
      val live = scala.collection.mutable.Set[Int]()
      for (cycle <- 0 until 400) {
        // 1) Build the request prefix first and poke it.
        val reqK = rng.nextInt(allocWidth + 1)
        for (i <- 0 until allocWidth) { dut.io.allocateReq(i).poke((i < reqK).B) }
        // 2) Only now sample the pre-edge canAllocate and slots for that prefix.
        val can  = preEdgeCan(dut, allocWidth)
        val slot = (0 until allocWidth).map(i => dut.io.newSlot(i).peek().litValue.toInt)
        // 3) Accept only requested lanes that can allocate.
        val doAlloc = (0 until allocWidth).map(i => i < reqK && can(i) == 1)
        for (i <- 0 until allocWidth) { dut.io.doAllocate(i).poke(doAlloc(i).B) }
        // 4) Free only currently-live entries.
        val toFree = live.toSeq.filter(_ => rng.nextInt(4) == 0)
        dut.io.free.poke(toFree.foldLeft(BigInt(0))((a, idx) => a | (BigInt(1) << idx)).U(size.W))
        // 5) Edge, then compare.
        dut.clock.step(1)
        compare(dut, allocWidth, cycle, "legal")
        // 6) Oracle update at the correct edge: accepted allocations are added
        // (and must never duplicate a live index), then the freed entries leave.
        for (i <- 0 until allocWidth if doAlloc(i)) {
          assert(!live.contains(slot(i)), s"duplicate allocation of ${slot(i)} at $cycle")
          live += slot(i)
        }
        toFree.foreach(live -= _)
      }
    }
    ()
  }
}
