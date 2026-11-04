// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}

/** Select ordinary QueueModel circuit generation. */
case object QueueRTL extends Field[Boolean](true)

/** Owns the circular enqueue/dequeue pointers; subclasses provide payload storage. */
abstract class QueueBase[T <: Data](val entries: Int)(
  implicit val p: Parameters,
  val expandRTL: Field[Boolean]
) extends Module {
  require(entries > 0, "Queue capacity must be positive")

  /** Use the wrap phase to distinguish full from empty at the same index.
    * A single-slot queue needs only the phase bit.
    */
  protected class QueuePtr extends CircularQueuePtr[QueuePtr](QueueBase.this.entries) {
    override val value = UInt(log2Ceil(QueueBase.this.entries).W)
  }

  protected def enqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)
  protected def deqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)

  protected val enqPtr: QueuePtr = RegInit(enqPtrInit)
  protected val deqPtr: QueuePtr = RegInit(deqPtrInit)

  /** Read all physical slots now; apply any validity guard in the callback. */
  protected def foreach(read: T => Unit): Unit

  /** Register per-entry writes for later emission. Reads see current state;
    * later enabled writes win per field.
    */
  protected def update(modify: T => Unit): Unit

  /** Register elements and acceptance for later enqueue emission. Accepted
    * elements occupy successive positions from the old tail in input order;
    * their population count determines the tail advance. A later registration
    * replaces the earlier definition. Admission belongs to the concrete owner.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit

  /** Test all physical slots; include any validity check in the predicate. */
  protected def exists(predicate: T => Bool): Bool
}

/** Derive occupancy from the live interval [deqPtr, enqPtr).
  * Requires a pointer distance of at most `entries`.
  */
trait QueueStatus extends HasCircularQueuePtrHelper { this: QueueBase[_] =>
  protected final def count: UInt = {
    val result = Wire(UInt(log2Ceil(entries + 1).W))
    result := distanceBetween(enqPtr, deqPtr)
    result
  }

  protected final def empty: Bool = isEmpty(enqPtr, deqPtr)
  protected final def full: Bool = isFull(enqPtr, deqPtr)
}
