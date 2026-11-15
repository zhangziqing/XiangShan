// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.reflect.DataMirror

/** A validated member path, invariant in both the entry and selected Data types.
  * The descriptor stores no callback and cannot describe the entire entry.
  */
final class FieldOf[T <: Data, D <: Data] private (
  private val prototype: T,
  val path: Vector[FieldOf.Step]
) {
  /** Locate the member on an entry of the same structural Chisel type. */
  def select(entry: T): D = {
    require(DataMirror.checkTypeEquivalence(prototype, entry),
      "FieldOf entry type does not match its prototype")
    val member = path.foldLeft[Data](entry) {
      case (record: Record, FieldOf.Field(name)) => record.elements(name)
      case (vector: Vec[_], FieldOf.Index(index)) => vector(index)
      case _ => throw new IllegalArgumentException("FieldOf path does not match entry")
    }
    // Only the companion can construct a descriptor. Its identity check fixes
    // D to this path, and structural equivalence preserves that correspondence.
    member.asInstanceOf[D]
  }
}

object FieldOf {
  sealed trait Step
  final case class Field(name: String) extends Step
  final case class Index(value: Int) extends Step

  /** Execute the selector once on an unbound type, then discard it.
    * This checks membership at elaboration, not arbitrary Scala side effects.
    */
  def apply[T <: Bundle, D <: Data](gen: T)(selector: T => D): FieldOf[T, D] = {
    val prototype = gen.cloneType
    val selected = selector(prototype)

    def find(node: Data, path: Vector[Step]): Option[Vector[Step]] = {
      if (node eq selected) Some(path)
      else node match {
        case record: Record => record.elements.iterator.flatMap { case (name, child) =>
          find(child, path :+ Field(name))
        }.take(1).toList.headOption
        case vector: Vec[_] => vector.getElements.iterator.zipWithIndex.flatMap { case (child, index) =>
          find(child, path :+ Index(index))
        }.take(1).toList.headOption
        case _ => None
      }
    }

    val path = find(prototype, Vector.empty).getOrElse {
      throw new IllegalArgumentException("FieldOf selector must return a static member of its entry")
    }
    require(path.nonEmpty, "FieldOf selector must not return the whole entry")
    new FieldOf[T, D](prototype, path)
  }
}
