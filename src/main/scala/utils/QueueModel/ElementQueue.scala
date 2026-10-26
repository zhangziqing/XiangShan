// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import org.chipsalliance.cde.config.{Field, Parameters}

/** Queue-owned entry state and circular endpoints, without an admission policy.
  * Specializations define membership, pointer transitions and entry behavior.
  * Read callbacks expand immediately against old state; write callbacks compose
  * in registration order and expand once at the final construction boundary.
  * Every callback visits every physical entry, including unallocated entries.
  * Callers supply their own validity, address and reset guards.
  */
abstract class ElementQueue[T <: Data](gen: T, entries: Int)(
  implicit p: Parameters, expandRTL: Field[Boolean]
) extends QueueBase[T](entries) {
  require(p(expandRTL), "Alternative Queue circuit generation has not been defined")

  /** Evaluated during superclass construction. Partial literals reset only
    * specified fields; other payload fields remain unreset, including writes
    * concurrent with reset. Contents do not determine pointer membership.
    */
  protected def initContents: Option[Seq[T]] = None

  private val contents: Vec[T] = initContents match {
    case Some(initial) =>
      require(initial.length == entries, "initial contents must describe exactly one entry per slot")
      RegInit(VecInit(initial))
    case None => Reg(Vec(entries, gen))
  }
  private var modifyElement: T => Unit = _ => ()
  private var emitted = false

  protected final def compose(first: T => Unit, second: T => Unit): T => Unit =
    entry => { first(entry); second(entry) }

  override protected final def foreach(read: T => Unit): Unit = contents.foreach(read)

  /** Conditions must be inside the callback: ambient Chisel when scopes are
    * not captured for later expansion. Field-wise last-enabled-write priority
    * applies; every right-hand-side expression still reads the old entry.
    */
  override protected final def update(modify: T => Unit): Unit = {
    require(!emitted, "element updates must be registered before emission")
    modifyElement = compose(modifyElement, modify)
  }

  override protected final def exists(predicate: T => Bool): Bool = {
    val result = WireDefault(false.B)
    foreach { entry => when(predicate(entry)) { result := true.B } }
    result
  }

  /** Call once after all inherited and concrete update registrations, while
    * constructing the owning Module. This does not gate writes on reset.
    */
  protected final def emitElementUpdates(): Unit = {
    require(!emitted, "element updates must be emitted exactly once")
    emitted = true
    foreach(modifyElement)
  }

  /** Elaboration-time slot identity for generic QueueModel circuit lowering.
    * This returns neither entry state nor an addressable storage handle. Entry
    * observations and writes still occur exclusively in foreach/update bodies.
    * External specializations can carry an immutable slot field in their entry.
    */
  private[QueueModel] final def physicalSlot(entry: T): Int = {
    val slot = contents.indexWhere(_ eq entry)
    require(slot >= 0, "slot identity requires this Queue's callback entry")
    slot
  }
}
