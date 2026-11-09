// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}

case object QueueRTL extends Field[Boolean](true)

/** Shared queue interfaces; subclasses own storage and emission. */
abstract class QueueBase[T <: Data](val entries: Int)(
  implicit val p: Parameters,
  val expandRTL: Field[Boolean]
) extends Module {
  require(entries > 0, "Queue capacity must be positive")

  /** The phase distinguishes full from empty; a single slot has no index bits. */
  protected class QueuePtr extends CircularQueuePtr[QueuePtr](QueueBase.this.entries) {
    override val value = UInt(log2Ceil(QueueBase.this.entries).W)
  }

  // Reset hooks run before subclass fields initialize.
  protected def enqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)
  protected def deqPtrInit: QueuePtr = 0.U.asTypeOf(new QueuePtr)

  protected val enqPtr: QueuePtr = RegInit(enqPtrInit)
  protected val deqPtr: QueuePtr = RegInit(deqPtrInit)

  /** Read-only current state; the index must address a physical slot. */
  protected def read(index: Int): T
  protected def read(index: UInt): T

  /** Indexed writes do not advance either pointer or perform admission checks. */
  protected def write(index: UInt, data: T, enable: Bool): Unit

  /** field selects a static path within the entry without side effects. */
  protected def writeField[D <: Data](index: UInt, field: T => D, data: D, enable: Bool): Unit

  /** Read-only traversal without implicit validity filtering. */
  protected def foreach(read: T => Unit): Unit

  /** Apply in the current Chisel scope; RHS reads old state and later enabled writes win per field. */
  protected def update(modify: T => Unit): Unit

  /** Uses the same read-only, unfiltered observation contract as foreach. */
  protected def exists(predicate: T => Bool): Bool

  /** Return one acceptance bit per input; invalid inputs must be rejected. */
  protected def enqGuard(elements: Seq[ValidIO[T]]): Seq[Bool]

  /** Gates consumption; reads remain visible. */
  protected def deqGuard(count: UInt): Bool

  /** Generate enqueue once per instance and return this call's acceptance. */
  protected def enq(elements: Seq[ValidIO[T]]): Seq[Bool]

  /** Bypasses enqGuard; accepted inputs pack from the old tail in port order.
    * Call once per instance. Conflicting writes follow Chisel connection order.
    */
  protected def enq(elements: Seq[T], accepted: Seq[Bool]): Unit

  /** Read from the old head; callers keep consumption within current occupancy. */
  protected def deq(count: UInt, offsets: Seq[UInt]): Seq[T]

  /** Normally use enq; this bypasses admission and payload writes. */
  protected def advanceEnq(count: UInt): Unit

  /** Normally use deq; this bypasses admission and payload reads. */
  protected def advanceDeq(count: UInt): Unit

  protected def onRecover(): Unit
  protected def onFlush(): Unit
}

/** Requires a dense pointer interval of at most entries; ignores per-slot validity. */
trait QueueStatus extends HasCircularQueuePtrHelper { this: QueueBase[_] =>
  protected final def count: UInt = {
    val result = Wire(UInt(log2Ceil(entries + 1).W))
    result := distanceBetween(enqPtr, deqPtr)
    result
  }

  protected final def empty: Bool = isEmpty(enqPtr, deqPtr)
  protected final def full: Bool = isFull(enqPtr, deqPtr)
}
