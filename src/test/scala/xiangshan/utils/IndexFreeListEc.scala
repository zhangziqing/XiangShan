// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.utils

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.mem.FreeList
import _root_.utils.QueueModel.{IndexFreeList, QueueRTL}

/** Identical standalone boundaries, with every payload/control/event exposed.
  * No request tie-offs, payload masking, or environment assumptions.
  */
class FreeListEcTop(implementation: Boolean, size: Int, width: Int, returns: Int,
                    prealloc: Boolean, asyncReset: Boolean)
    (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val allocateReq = Input(Vec(width, Bool()))
    val doAllocate = Input(Vec(width, Bool()))
    val free = Input(UInt(size.W))
    val allocateSlot = Output(Vec(width, UInt(log2Up(size).W)))
    val canAllocate = Output(Vec(width, Bool()))
    val validCount = Output(UInt(log2Ceil(size + 1).W))
    val empty = Output(Bool())
    val perf = Output(UInt(6.W))
  })
  if (implementation) {
    val allocator = withReset(if (asyncReset) reset.asAsyncReset else reset) {
      Module(new IndexFreeList(size, width, returns, prealloc)(p, QueueRTL))
    }
    allocator.io.allocateReq := io.allocateReq
    allocator.io.doAllocate := io.doAllocate
    allocator.io.free := io.free
    io.allocateSlot := allocator.io.allocateSlot
    io.canAllocate := allocator.io.canAllocate
    io.validCount := allocator.io.validCount
    io.empty := allocator.io.empty
    io.perf := allocator.getPerf(0).value
  } else {
    val allocator = withReset(if (asyncReset) reset.asAsyncReset else reset) {
      Module(new FreeList(size, width, returns, prealloc))
    }
    allocator.io.allocateReq := io.allocateReq
    allocator.io.doAllocate := io.doAllocate
    allocator.io.free := io.free
    io.allocateSlot := allocator.io.allocateSlot
    io.canAllocate := allocator.io.canAllocate
    io.validCount := allocator.io.validCount
    io.empty := allocator.io.empty
    io.perf := allocator.getPerf(0).value
  }
}

/** Emit the actual RTL pair for each production geometry and width boundaries. */
object IndexFreeListEc extends App {
  require(args.length == 1 || (args.length == 2 && args(1) == "--async-reset"),
    "usage: IndexFreeListEc <new-output-directory> [--async-reset]")
  val asyncReset = args.length == 2
  val root = Paths.get(args(0))
  require(!Files.exists(root), "preserve earlier generated inputs")
  Files.createDirectories(root)
  val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.CRITICAL, 0)
  }
  val c = p(XSCoreParamsKey)
  val cases = Seq(
    ("rar", c.LoadQueueRARSize, c.LoadPipelineWidth, 4, false),
    ("raw", c.LoadQueueRAWSize, c.LoadPipelineWidth, 4, true),
    ("replay", c.LoadQueueReplaySize, c.LoadPipelineWidth, 4, true),
    ("uncache", c.LoadUncacheBufferSize, c.LoadPipelineWidth, 4, true),
    ("vl", c.VlMergeBufferSize, c.VecLoadPipelineWidth, c.VLUopWritebackWidth, false),
    ("vs", c.VsMergeBufferSize, c.VecStorePipelineWidth, c.VSUopWritebackWidth, false),
    ("small", 6, 3, 2, false), ("small-prealloc", 6, 3, 2, true),
    ("one-lane", 2, 1, 1, false), ("one-lane-prealloc", 2, 1, 1, true)
  )
  val records = cases.map { case (name, size, width, returns, prealloc) =>
    val dir = root.resolve(name)
    Files.createDirectory(dir)
    for ((label, implementation) <- Seq("reference" -> false, "implementation" -> true)) {
      val rtl = ChiselStage.emitSystemVerilog(
        new FreeListEcTop(implementation, size, width, returns, prealloc, asyncReset),
        firtoolOpts = Array("-O=release", "--disable-all-randomization", "--strip-debug-info"))
      Files.writeString(dir.resolve(s"$label.sv"), rtl)
    }
    s"{\"name\":\"$name\",\"size\":$size,\"allocWidth\":$width,\"freeWidth\":$returns,\"prealloc\":$prealloc,\"asyncReset\":$asyncReset}"
  }
  Files.writeString(root.resolve("cases.json"), records.mkString("[\n", ",\n", "\n]\n"))
}
