// PHT queue miter: live production QueueModel.OverrideableQueue children vs
// baseline utils.OverrideableQueue.
package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chiseltest._ // scalastyle:ignore
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._
import chisel3.reflect.DataMirror
import chisel3.ActualDirection
import _root_.utils.OverrideableQueue

/** Exposes the live production lookup/evict QueueModel.OverrideableQueue ports
  * and overrides the parent consumer ready by last-connect in the subclass. */
class TestablePHT(implicit p: Parameters) extends PatternHistoryTable {
  // Normally supplied by the enclosing MemBlock SRAM broadcast network.
  pht_ram.io.broadcast.foreach(b => b := 0.U.asTypeOf(b))
  val lookupReady = IO(Input(Bool())); val evictReady = IO(Input(Bool()))
  val probe = IO(new Bundle {
    val lookupEnqValid = Output(Bool()); val lookupEnqData = Output(new PhtLookup)
    val lookupDeqValid = Output(Bool()); val lookupDeqData = Output(new PhtLookup)
    val evictDeqValid = Output(Bool()); val evictDeqData = Output(new AGTEntry)
  })
  probe.lookupEnqValid := lookup_queue.io.in.valid
  probe.lookupEnqData  := lookup_queue.io.in.bits
  probe.lookupDeqValid := lookup_queue.io.out.valid
  probe.lookupDeqData  := lookup_queue.io.out.bits
  lookup_queue.io.out.ready := lookupReady
  probe.evictDeqValid := evict_queue.io.out.valid
  probe.evictDeqData  := evict_queue.io.out.bits
  evict_queue.io.out.ready := evictReady
}

/** Baseline reference: the pre-migration utils.OverrideableQueue with the same
  * inputs. Production now uses the independent QueueModel implementation. */
class PhtQueueRef(size: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val lookupIn = Flipped(Valid(new PhtLookup)); val lookupReady = Input(Bool())
    val lookupOutValid = Output(Bool()); val lookupOut = Output(new PhtLookup)
    val evictIn = Flipped(Valid(new AGTEntry)); val evictReady = Input(Bool())
    val evictOutValid = Output(Bool()); val evictOut = Output(new AGTEntry)
  })
  val lq = Module(new OverrideableQueue(new PhtLookup, size))
  lq.io.in := io.lookupIn
  lq.io.out.ready := io.lookupReady
  io.lookupOutValid := lq.io.out.valid; io.lookupOut := lq.io.out.bits
  val eq = Module(new OverrideableQueue(new AGTEntry, size))
  eq.io.in := io.evictIn
  eq.io.out.ready := io.evictReady
  io.evictOutValid := eq.io.out.valid; io.evictOut := eq.io.out.bits
}

class PhtQueueMiterHarness(size: Int)(implicit p: Parameters) extends XSModule {
  private val pht = Module(new TestablePHT)
  private val ref = Module(new PhtQueueRef(size))
  val io = IO(new Bundle {
    val lookupIn = Flipped(Valid(new PhtLookup))
    val evictIn = Flipped(Valid(new AGTEntry))
    val lookupReady = Input(Bool()); val evictReady = Input(Bool())
    val mismatch = Output(Bool())
    val phtLookupHead = Output(UInt((new PhtLookup).pht_index.getWidth.W))
    val phtEvictHeadIndex = Output(UInt((new AGTEntry).pht_index.getWidth.W))
    val phtEvictValid = Output(Bool())
    val phtLookupValid = Output(Bool())
    val refLookupValid = Output(Bool())
  })
  pht.io.s2_agt_lookup := io.lookupIn
  pht.io.agt_update := io.evictIn
  pht.lookupReady := io.lookupReady
  pht.evictReady := io.evictReady
  ref.io.lookupIn := io.lookupIn
  ref.io.evictIn := io.evictIn
  ref.io.lookupReady := io.lookupReady
  ref.io.evictReady := io.evictReady
  private def lookupEq = (pht.probe.lookupDeqValid ^ ref.io.lookupOutValid) ||
    ((pht.probe.lookupDeqValid && ref.io.lookupOutValid) &&
      (pht.probe.lookupDeqData.asUInt =/= ref.io.lookupOut.asUInt))
  private def evictEq = (pht.probe.evictDeqValid ^ ref.io.evictOutValid) ||
    ((pht.probe.evictDeqValid && ref.io.evictOutValid) &&
      (pht.probe.evictDeqData.asUInt =/= ref.io.evictOut.asUInt))
  io.phtLookupHead := pht.probe.lookupDeqData.pht_index
  io.phtEvictHeadIndex := pht.probe.evictDeqData.pht_index
  io.phtEvictValid := pht.probe.evictDeqValid
  io.phtLookupValid := pht.probe.lookupDeqValid
  io.refLookupValid := ref.io.lookupOutValid
  io.mismatch := lookupEq || evictEq
  when(!reset.asBool) { assert(!io.mismatch, "PHT queue mismatch") }
}

class PatternHistoryTableQueueSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => core
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.CRITICAL, 0)
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug, here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform, here(DebugOptionsKey).EnableXMR)
  })
  private val size = core.prefetcher.get.asInstanceOf[SMSParams].pht_lookup_queue_size

  it should "match baseline OverrideableQueue on overwrite-full, drain, wrap and mid-reset" in {
    test(new PhtQueueMiterHarness(size)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      // input init before reset
      def initInputs(data: Data): Unit = data match {
        case v: Vec[_] => v.foreach(initInputs)
        case r: Record => r.elements.values.foreach(initInputs)
        case b: Bool   => if (DataMirror.directionOf(b) == ActualDirection.Input) b.poke(false.B)
        case u: UInt   => if (DataMirror.directionOf(u) == ActualDirection.Input) u.poke(0.U)
        case sn: SInt  => if (DataMirror.directionOf(sn) == ActualDirection.Input) sn.poke(0.S)
        case other => require(DataMirror.directionOf(other) != ActualDirection.Input, s"unsupported input: $other")
      }
      initInputs(dut.io)
      dut.io.lookupReady.poke(false.B); dut.io.evictReady.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      def edge(): Unit = { dut.clock.step(1); assert(dut.io.mismatch.peek().litValue == 0, "PHT queue mismatch") }

      // fill depth+1 with ready low: head must be overwritten to the extra request
      for (k <- 0 until size + 1) {
        dut.io.lookupIn.valid.poke(true.B); dut.io.lookupIn.bits.pht_index.poke((k + 1).U)
        dut.io.evictIn.valid.poke(true.B); dut.io.evictIn.bits.pht_index.poke((k + 1).U)
        edge()
      }
      dut.io.lookupIn.valid.poke(false.B); dut.io.evictIn.valid.poke(false.B)
      edge()
      assert(dut.io.mismatch.peek().litValue == 0, "overwrite-full diverged")
      // extra (depth+1-th) offer must be the current head on both sides
      assert(dut.io.phtLookupValid.peek().litValue != 0, "head must be valid after fill")
      assert(dut.io.phtLookupHead.peek().litValue == (size + 1), "extra offer must overwrite the oldest head")
      assert(dut.io.refLookupValid.peek().litValue == dut.io.phtLookupValid.peek().litValue, "head valid differs")
      dut.io.phtEvictValid.expect(true.B)
      dut.io.phtEvictHeadIndex.expect((size + 1).U)
      // Drain exactly the retained capacity at independent endpoints.
      dut.io.lookupReady.poke(true.B); dut.io.evictReady.poke(true.B)
      var lookupFires = 0; var evictFires = 0
      for (_ <- 0 until size) {
        if (dut.io.phtLookupValid.peek().litToBoolean) lookupFires += 1
        if (dut.io.phtEvictValid.peek().litToBoolean) evictFires += 1
        edge()
      }
      assert(lookupFires == size && evictFires == size)
      dut.io.phtLookupValid.expect(false.B); dut.io.phtEvictValid.expect(false.B)
      // wrap: refill
      dut.io.lookupReady.poke(false.B); dut.io.evictReady.poke(false.B)
      for (k <- 0 until size) {
        dut.io.lookupIn.valid.poke(true.B); dut.io.lookupIn.bits.pht_index.poke((0x08 + k).U)
        dut.io.evictIn.valid.poke(true.B); dut.io.evictIn.bits.pht_index.poke((0x08 + k).U)
        edge()
      }
      dut.io.lookupIn.valid.poke(false.B); dut.io.evictIn.valid.poke(false.B)
      // mid-reset: assert reset while traffic is live
      dut.io.lookupIn.valid.poke(true.B); dut.io.lookupIn.bits.pht_index.poke(0x1f.U)
      dut.reset.poke(true.B); edge(); dut.reset.poke(false.B)
      dut.io.lookupIn.valid.poke(false.B)
      edge(); edge()
      dut.io.mismatch.expect(false.B)
    }
  }
}
