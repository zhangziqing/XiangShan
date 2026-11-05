/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

import org.chipsalliance.cde.config.Parameters
import chisel3.experimental.requireIsChiselType
import chisel3.reflect.DataMirror
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache._
import utils.QueueModel.{QueueRTL, RingQueue}
import difftest._

class DatamoduleResultBufferIO[T <: Data](gen: T)(implicit p: Parameters) extends XSBundle
{
  // val flush = Input(Bool())
  val enq = Vec(EnsbufferWidth, Flipped(DecoupledIO(gen)))
  val deq = Vec(EnsbufferWidth, DecoupledIO(gen))

}

class DatamoduleResultBuffer[T <: Data]
(
  gen: T,
)(implicit p: Parameters) extends RingQueue[T](
  gen,
  p(XSCoreParamsKey).EnsbufferWidth,
  p(XSCoreParamsKey).EnsbufferWidth,
  p(XSCoreParamsKey).EnsbufferWidth
)(p, QueueRTL) {

  private val n = p(XSCoreParamsKey).EnsbufferWidth

  requireIsChiselType(gen)

  val io = IO(new DatamoduleResultBufferIO[T](gen))

  if (isPow2(n)) {
  /** Baseline contracts: enqueue valid lanes and dequeue ready lanes are prefixes.
    * Read the old endpoint state, then release the prefix that actually fires and
    * reuse those slots for this cycle's accepted enqueues. RingQueue applies
    * enqueue writes after dequeue advancement, so a same-slot replacement wins,
    * matching the baseline clear-then-write order.
    */
  val readyPrefix = Wire(Vec(n, Bool()))
  readyPrefix(0) := io.deq(0).ready
  for (i <- 1 until n) {
    readyPrefix(i) := readyPrefix(i - 1) && io.deq(i).ready
  }
  val readyCount = PopCount(readyPrefix)
  val deqCount = Mux(count >= readyCount, readyCount, count)
  val heads = deq(deqCount, (0 until n).map(_.U))

  val validPrefix = Wire(Vec(n, Bool()))
  validPrefix(0) := io.enq(0).valid
  for (i <- 1 until n) {
    validPrefix(i) := validPrefix(i - 1) && io.enq(i).valid
  }
  val free = n.U + deqCount - count
  val accepted = (0 until n).map(i => validPrefix(i) && i.U < free)
  enq((0 until n).map(i => io.enq(i).bits).toSeq, accepted)

  for (i <- 0 until n) {
    // Occupancy is independent of the consumer's ready: a stalled queue still
    // exposes its full valid prefix. Ready only controls this cycle's release.
    io.deq(i).valid := i.U < count
    io.deq(i).bits := heads(i)
    // A lane is ready when its slot is free now or is released by this cycle's
    // dequeue prefix. Both readiness and acceptance use the same prefix.
    io.enq(i).ready := i.U < free
  }

  // Retain the baseline's prefix contracts on both directions. The two input
  // prefixes are caller obligations; the two output prefixes follow from the
  // implementation and are checked here as in the baseline.
  for (i <- 1 until n) {
    assert(!(io.deq(i).valid && !io.deq(i - 1).valid))
    assert(!(io.deq(i).ready && !io.deq(i - 1).ready))
    assert(!(io.enq(i).ready && !io.enq(i - 1).ready))
    assert(!(io.enq(i).valid && !io.enq(i - 1).valid))
  }

    emitRTL()

  } else {
    // Retain the exact legacy binary-index ring for non-power-of-two geometries,
    // whose truncated index behaviour is not the same as capacity wrap.
    val data = Reg(Vec(n, gen))
    val valids = RegInit(VecInit(Seq.fill(n)(false.B)))
    val enq_flag = RegInit(0.U(log2Up(n).W))
    val deq_flag = RegInit(0.U(log2Up(n).W))
    val entry_allowin = Wire(Vec(n, Bool()))

    for (index <- 0 until n) {
      io.deq(index).valid := valids(deq_flag + index.U) && (if (index == 0) 1.B else io.deq(index - 1).valid)
      io.deq(index).bits := data(deq_flag + index.U)
    }
    for (index <- 0 until n) {
      entry_allowin(index) := !valids(index) ||
        (0 until n).map(i => io.deq(i).fire && deq_flag + i.U === index.U).reduce(_ || _)
    }
    for (index <- 0 until n) {
      io.enq(index).ready := entry_allowin(enq_flag + index.U) &&
        (if (index == 0) 1.B else io.enq(index - 1).ready)
    }
    for (i <- 1 until n) {
      assert(!(io.deq(i).valid && !io.deq(i - 1).valid))
      assert(!(io.deq(i).ready && !io.deq(i - 1).ready))
      assert(!(io.enq(i).ready && !io.enq(i - 1).ready))
      assert(!(io.enq(i).valid && !io.enq(i - 1).valid))
    }
    for (index <- 0 until n) {
      when(io.deq(index).fire) {
        valids(deq_flag + index.U) := 0.B
        if (n > 1) deq_flag := deq_flag + index.U + 1.U
      }
    }
    for (index <- 0 until n) {
      when(io.enq(index).fire) {
        valids(enq_flag + index.U) := 1.B
        data(enq_flag + index.U) := io.enq(index).bits
        if (n > 1) enq_flag := enq_flag + index.U + 1.U
      }
    }
  }
}
