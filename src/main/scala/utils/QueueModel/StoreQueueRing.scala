// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Owner-local ring configuration for StoreQueue.
  *
  * The parent keeps every original pointer, redirect/rollback policy and the
  * external SQData/SQAddr banks. Indexed storage accesses use the inherited
  * read/writeField API directly; no extra slot wrapper, public writable Vec,
  * per-entry slot field, or second set of endpoints is introduced.
  */
abstract class StoreQueueRing[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters
) extends RingQueue[T](gen, entries, 1, 1)(p, QueueRTL) {

  // This owner supplies its own admission, pointers and data banks. Q-G03 still
  // requires a final enqueue definition before emission; the explicit all-false
  // registration keeps only that final enqueue inactive (ordinary deq is used).
  enq(Seq(0.U.asTypeOf(gen)), Seq(false.B))
}
