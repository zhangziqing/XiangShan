// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.utils

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.mem.FreeList
import _root_.utils.QueueModel.{IndexFreeList, QueueRTL}
import scala.util.Random

/** Candidates use all-true requests and rank sparse physical consumers.
  * The old allocator consumes PopCount(accepted); the new binding supplies
  * that count as a prefix. The original FreeList is the independent reference.
  */
class QueueModelFreeListMiter(size: Int, width: Int, freeWidth: Int, prealloc: Boolean)
    (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val accepted = Input(Vec(width, Bool()))
    val free = Input(UInt(size.W))
    val candidate = Output(Vec(width, UInt(log2Ceil(size).W)))
    val available = Output(Vec(width, Bool()))
    val count = Output(UInt(log2Ceil(size + 1).W))
    val empty = Output(Bool())
  })
  val model = Module(new IndexFreeList(size, width, freeWidth, prealloc)(p, QueueRTL))
  val reference = Module(new FreeList(size, width, freeWidth, prealloc, "QueueModelReference"))
  for (i <- 0 until width) {
    model.io.allocateReq(i) := true.B
    reference.io.allocateReq(i) := true.B
    model.io.doAllocate(i) := i.U < PopCount(io.accepted)
    reference.io.doAllocate(i) := io.accepted(i)
    io.candidate(i) := model.io.allocateSlot(i)
    io.available(i) := model.io.canAllocate(i)
    assert(model.io.canAllocate(i) === reference.io.canAllocate(i))
    when(model.io.canAllocate(i)) {
      assert(model.io.allocateSlot(i) === reference.io.allocateSlot(i))
    }
  }
  model.io.free := io.free
  reference.io.free := io.free
  io.count := model.io.validCount
  io.empty := model.io.empty
  assert(model.io.validCount === reference.io.validCount)
  assert(model.io.empty === reference.io.empty)
}

class QueueModelFreeListSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => core
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false,
      perfLevel = XSPerfLevel.CRITICAL, perfDBHartID = 0)
  }

  private def run(size: Int, width: Int, returns: Int, prealloc: Boolean, seed: Long): Unit = {
    test(new QueueModelFreeListMiter(size, width, returns, prealloc))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.accepted.foreach(_.poke(false.B))
      dut.io.free.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(4)
      dut.reset.poke(false.B); dut.clock.step()
      dut.io.count.expect(0.U); dut.io.empty.expect(false.B)
      val live = scala.collection.mutable.Set.empty[Int]
      val random = new Random(seed)
      for (cycle <- 0 until 600) {
        dut.io.accepted.foreach(_.poke(false.B))
        val freeSlots = size - dut.io.count.peek().litValue.toInt
        val candidates = dut.io.available.takeWhile(_.peek().litToBoolean).length
        val n = random.nextInt(math.min(candidates, freeSlots) + 1)
        val lanes = random.shuffle((0 until width).toList).take(n).sorted
        val selected = (0 until n).map { rank =>
          dut.io.available(rank).expect(true.B)
          dut.io.candidate(rank).peek().litValue.toInt
        }
        val returning = live.toVector.sorted.filter(_ => random.nextInt(6) == 0)
        lanes.foreach(i => dut.io.accepted(i).poke(true.B))
        dut.io.free.poke(returning.foldLeft(BigInt(0))((m, i) => m.setBit(i)).U)
        selected.foreach(i => assert(!live(i), s"duplicate index $i at $cycle"))
        dut.clock.step()
        returning.foreach(live.remove)
        selected.foreach(live.add)
        assert(dut.io.count.peek().litValue >= live.size)
      }
      // Settle pending returns, then exhaust to measure exact reclaim latency.
      dut.io.free.poke(0.U); dut.io.accepted.foreach(_.poke(false.B))
      dut.clock.step(5)
      var remaining = size - dut.io.count.peek().litValue.toInt
      while (remaining > 0) {
        val n = math.min(width, remaining)
        dut.io.accepted.zipWithIndex.foreach { case (x, i) => x.poke((i < n).B) }
        dut.clock.step()
        remaining = size - dut.io.count.peek().litValue.toInt
      }
      dut.io.accepted.foreach(_.poke(false.B)); dut.io.empty.expect(true.B)
      dut.io.free.poke(1.U); dut.clock.step(); dut.io.free.poke(0.U)
      dut.io.empty.expect(true.B); dut.clock.step(); dut.io.empty.expect(true.B)
      dut.clock.step(); dut.io.empty.expect(false.B)
      dut.reset.poke(true.B); dut.clock.step(2)
      dut.reset.poke(false.B); dut.clock.step()
      dut.io.count.expect(0.U); dut.io.empty.expect(false.B)
    }
  }

  behavior of "QueueModel production allocator binding"
  private val c = p(XSCoreParamsKey)
  for ((name, size, width, returns, prealloc) <- Seq(
    ("RAR", c.LoadQueueRARSize, c.LoadPipelineWidth, 4, false),
    ("RAW", c.LoadQueueRAWSize, c.LoadPipelineWidth, 4, true),
    ("Replay", c.LoadQueueReplaySize, c.LoadPipelineWidth, 4, true),
    ("Uncache", c.LoadUncacheBufferSize, c.LoadPipelineWidth, 4, true),
    ("VL", c.VlMergeBufferSize, c.VecLoadPipelineWidth, c.VLUopWritebackWidth, false),
    ("VS", c.VsMergeBufferSize, c.VecStorePipelineWidth, c.VSUopWritebackWidth, false),
    ("small", 6, 3, 2, false),
    ("small-prealloc", 6, 3, 2, true)
  )) {
    it should s"preserve $name candidates, sparse consumers, reclaim, reset and wrap" in {
      run(size, width, returns, prealloc, name.hashCode.toLong)
    }
  }
}
