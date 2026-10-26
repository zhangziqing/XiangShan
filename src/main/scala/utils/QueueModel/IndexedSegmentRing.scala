// SPDX-License-Identifier: MulanPSL-2.0
// Protected indexed payload access for the RingQueue core.
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

/** RingQueue extension for queues that address payload slots directly.
  *
  * The core still owns storage, transfer and pointer state. This extension adds
  * only:
  *   - `indexedRead`: a combinational, un-gated view of one physical slot;
  *   - `indexedUpdate`: field-wise writes at one physical slot.
  *
  * `indexedUpdate` emits before `emitRTL`'s deferred whole-entry enqueue batch.
  * A caller that needs merge-after-enqueue on a same-slot collision must fold
  * that field into the enqueue payload; field-only writes to a different slot
  * remain valid because the enqueue batch only writes its own slot. This
  * preserves the core contract that the final enqueue owns the whole entry.
  *
  * Neither method gates on reset or validity. Payload registers are unreset and
  * reset-concurrent writes are part of the caller-visible behavior.
  */
abstract class IndexedSegmentRing[T <: Data](
  gen: T,
  entries: Int,
  numEnq: Int = 1,
  numDeq: Int = 1
)(implicit p: Parameters, expandRTL: Field[Boolean])
  extends RingQueue[T](gen, entries, numEnq, numDeq) {

  /** Combinational raw view of the physical slot at `addr`. The caller owns
    * address legality; this method adds no validity or reset guard.
    */
  protected final def indexedRead(addr: UInt): T = {
    val result = Wire(gen)
    result := 0.U.asTypeOf(gen)
    foreach { entry => when(addr === physicalSlot(entry).U) { result := entry } }
    result
  }

  /** Field-wise write at one physical slot, emitted before the deferred
    * enqueue batch. Only fields assigned by `modify` are replaced; the caller
    * supplies the enable condition. Payload writes are not reset-gated.
    */
  protected final def indexedUpdate(addr: UInt, enable: Bool)(modify: T => Unit): Unit = {
    update { entry =>
      when(enable && addr === physicalSlot(entry).U) { modify(entry) }
    }
  }
}
