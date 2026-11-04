// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Owner-local indexed storage for VirtualLoadQueue.
  *
  * The parent keeps the original circular-pointer policy and protocol. This
  * adapter exposes absolute-slot reads and update callbacks without adding a
  * public writable Vec, a per-entry slot field, or a second set of endpoints.
  */
abstract class VirtualLoadQueueRing[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters
) extends RingQueue[T](gen, entries, 1, 1)(p, QueueRTL) {

  protected final def readSlot(index: Int): T = read(index)
  protected final def readSlot(index: UInt): T = read(index)

  protected final def updateSlot(slot: UInt)(modify: T => Unit): Unit = {
    update { entry =>
      when(physicalSlot(entry).U === slot) { modify(entry) }
    }
  }
}
