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

  require(isPow2(n), "DatamoduleResultBuffer requires a power-of-two EnsbufferWidth")
  requireIsChiselType(gen)

  val io = IO(new DatamoduleResultBufferIO[T](gen))

  // Both directions are prefix contracts. Release only the dequeue prefix that
  // actually fires, bounded by old occupancy.
  private val readyPrefix = Wire(Vec(n, Bool()))
  readyPrefix(0) := io.deq(0).ready
  for (i <- 1 until n) {
    readyPrefix(i) := readyPrefix(i - 1) && io.deq(i).ready
  }
  private val readyCount = PopCount(readyPrefix)
  private val deqCount = Mux(count >= readyCount, readyCount, count)
  private val heads = deq(deqCount, (0 until n).map(_.U))

  // Same-cycle dequeue credit: the freed slots can be reused by this cycle's
  // accepted enqueues.
  private val free = n.U + deqCount - count

  private val enqElements = Seq.tabulate(n) { i =>
    val element = Wire(Valid(gen))
    element.valid := io.enq(i).valid
    element.bits := io.enq(i).bits
    element
  }

  override protected def enqGuard(elements: Seq[ValidIO[T]]): Seq[Bool] = {
    val validPrefix = elements.map(_.valid).scanLeft(true.B)(_ && _).tail
    validPrefix.zipWithIndex.map { case (valid, index) =>
      valid && index.U < free && !recover && !flush
    }
  }

  // Native guarded enqueue: enqGuard owns admission, and the final enqueue
  // writes accepted entries after the dequeue read/advance registration.
  enq(enqElements)

  for (i <- 0 until n) {
    // Occupancy and payload are old-state; ready only describes this cycle's
    // free prefix, including slots released by the dequeue prefix above.
    io.deq(i).valid := i.U < count
    io.deq(i).bits := heads(i)
    io.enq(i).ready := i.U < free
  }

  for (i <- 1 until n) {
    assert(!(io.deq(i).valid && !io.deq(i - 1).valid))
    assert(!(io.deq(i).ready && !io.deq(i - 1).ready))
    assert(!(io.enq(i).ready && !io.enq(i - 1).ready))
    assert(!(io.enq(i).valid && !io.enq(i - 1).valid))
  }

  emitRTL()
}
