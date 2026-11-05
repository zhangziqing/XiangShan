// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Owner-local indexed storage for StoreQueue.
  *
  * The parent keeps every original pointer, redirect/rollback policy and the
  * external SQData/SQAddr banks. This adapter adds absolute-slot reads and
  * update callbacks without a public writable Vec, a per-entry slot field, or
  * a second set of endpoints.
  */
abstract class StoreQueueRing[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters
) extends RingQueue[T](gen, entries, 1, 1)(p, QueueRTL) {

  protected final def readSlot(index: Int): T = read(index)
  protected final def readSlot(index: UInt): T = read(index)

  protected final def updateSlot(slot: UInt, enable: Bool)(modify: T => Unit): Unit = {
    update { entry =>
      when(enable && physicalSlot(entry).U === slot) { modify(entry) }
    }
  }
}
