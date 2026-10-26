// SPDX-License-Identifier: MulanPSL-2.0
// Pointer-only Module foundation and pointer-derived Queue observations.
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}

/** Owns only the two circular pointers and declares element-access contracts.
  * Payload, IO, admission, access lowering and recovery belong to a concrete
  * Queue implementation.
  *
  * `p(expandRTL)` selects ordinary circuit expansion. The alternative circuit
  * is deliberately unspecified; implementations must reject it until supplied.
  * Reset-full places the tail one phase ahead of the head at the same index.
  */
abstract class QueueBase[T <: Data](val entries: Int)(
  implicit val p: Parameters,
  val expandRTL: Field[Boolean]
) extends Module {
  require(entries > 0, "Queue capacity must be positive")

  /** Bound to this Queue's capacity. `flag` is the circular ordering phase,
    * never an element-valid bit. A single-slot pointer has zero index bits,
    * leaving its phase as the entire pointer for inherited arithmetic.
    */
  protected class QueuePtr extends CircularQueuePtr[QueuePtr](QueueBase.this.entries) {
    override val value = UInt(log2Ceil(QueueBase.this.entries).W)
  }

  protected def enqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)
  protected def deqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)

  protected val enqPtrReg: QueuePtr = RegInit(enqPtrInit)
  protected val deqPtrReg: QueuePtr = RegInit(deqPtrInit)
  protected def enqPtr: QueuePtr = enqPtrReg
  protected def deqPtr: QueuePtr = deqPtrReg

  /** Observe current elements at the call site, without saving the callback.
    * The read-only callback supplies its own guards; the Queue adds no
    * membership filter.
    */
  protected def foreach(read: T => Unit): Unit

  /** Compose element updates under the pure-function assumption:
    * update(f); update(g) = update(compose(f, g)). Reads observe current state;
    * field-wise, last-enabled-write priority is preserved at the cycle edge.
    * The concrete implementation supplies an explicit circuit emission step.
    */
  protected def update(modify: T => Unit): Unit

  /** OR the caller's predicate over current storage elements. The predicate
    * supplies any validity test required by the concrete Queue contract.
    */
  protected def exists(predicate: T => Bool): Bool
}

/** Observations for a Queue whose live interval is [deqPtr, enqPtr).
  * The pointers must remain ordered with distance at most `entries`. This
  * trait adds combinational expressions only and cannot be mixed into an IO.
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
