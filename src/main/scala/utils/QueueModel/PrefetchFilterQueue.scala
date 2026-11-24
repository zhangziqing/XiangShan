// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

/** Shared storage initialization and admission for prefetch filter rings.
  *
  * Every slot resets to a zero `Valid` payload. Each enqueue element is an
  * outer offer Valid whose `valid` marks a required lane and whose inner Valid
  * is the value stored when that lane is accepted. Admission compares the old
  * tail plus the preceding required-lane count against the old head, so no
  * same-cycle dequeue or retirement gives credit. The outer data contract and
  * the owner's dequeue policy remain concrete: subclasses supply IO, offer
  * data, peeks and head handling, and call the native guarded enqueue.
  */
abstract class PrefetchFilterQueue[T <: Data](
  gen: T,
  entries: Int,
  numEnq: Int,
  numDeq: Int
)(implicit p: Parameters, expandRTL: Field[Boolean])
    extends RingQueue[ValidIO[T]](Valid(gen), entries, numEnq, numDeq) {

  override protected def initContents: Option[Seq[ValidIO[T]]] =
    Some(Seq.fill(entries)(0.U.asTypeOf(Valid(gen))))

  /** Required-lane admission uses old pointers and no retirement credit. */
  final override protected def enqGuard(elements: Seq[ValidIO[ValidIO[T]]]): Seq[Bool] = {
    elements.indices.map { i =>
      val needRank = PopCount(elements.take(i).map(_.valid))
      elements(i).valid && !recover && !flush && ((enqPtr + needRank) >= deqPtr)
    }
  }
}
