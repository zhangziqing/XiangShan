// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3.{Bundle, Data, Record, SInt, UInt, Vec}
import utils.QueueModel.FieldOf

object StorageLayout {
  val Packing = "chisel-asUInt-lsb-first-v1"

  def width(tpe: StorageType): Int = tpe match {
    case UIntType(w) => require(w > 0, "UInt width must be positive"); w
    case SIntType(w) => require(w > 0, "SInt width must be positive"); w
    case BundleType(fields) =>
      require(fields.nonEmpty, "empty bundles are unsupported")
      require(fields.map(_._1).distinct.size == fields.size, "duplicate bundle fields")
      require(fields.forall(_._1.nonEmpty), "empty field name")
      fields.foldLeft(0)((n, f) => Math.addExact(n, width(f._2)))
    case VecType(size, element) =>
      require(size > 0, "empty vectors are unsupported")
      Math.multiplyExact(size, width(element))
  }

  def apply(shape: StorageType): EntryLayout = {
    def flatten(tpe: StorageType, path: Vector[FieldStep], offset: Int): Vector[LeafLayout] = tpe match {
      case UIntType(w) => Vector(LeafLayout(path, offset, w, signed = false))
      case SIntType(w) => Vector(LeafLayout(path, offset, w, signed = true))
      case BundleType(fields) =>
        var next = offset
        fields.flatMap { case (name, child) =>
          val leaves = flatten(child, path :+ BundleField(name), next)
          next = Math.addExact(next, width(child))
          leaves
        }
      case VecType(size, child) =>
        (0 until size).toVector.flatMap(i => flatten(child, path :+ VecElement(i),
          Math.addExact(offset, Math.multiplyExact(i, width(child)))))
    }
    val total = width(shape)
    EntryLayout(shape, total, flatten(shape, Vector.empty, 0), Packing)
  }

  def fromData(data: Data): EntryLayout = {
    def shape(d: Data): StorageType = d match {
      case u: UInt => UIntType(u.getWidth)
      case s: SInt => SIntType(s.getWidth)
      case b: Bundle => BundleType(b.elements.toVector.map { case (name, child) => name -> shape(child) })
      case v: Vec[_] =>
        require(v.length > 0, "empty vectors are unsupported")
        val element = shape(v(0))
        require(v.getElements.forall(d => shape(d) == element), "heterogeneous vector")
        VecType(v.length, element)
      case _: Record => throw new IllegalArgumentException("custom/opaque Records are unsupported")
      case _ => throw new IllegalArgumentException(s"unsupported storage data type: ${d.getClass.getName}")
    }
    apply(shape(data))
  }

  /** Resolve a static member, including aggregate members, at generation time. */
  def field(layout: EntryLayout, path: Vector[FieldStep]): (StorageType, Int, Int) = {
    val (selected, offset) = path.foldLeft((layout.shape, 0)) {
      case ((BundleType(fields), base), BundleField(name)) =>
        val index = fields.indexWhere(_._1 == name)
        require(index >= 0, s"unknown bundle field: $name")
        val preceding = fields.take(index).foldLeft(0)((n, f) => Math.addExact(n, width(f._2)))
        (fields(index)._2, Math.addExact(base, preceding))
      case ((VecType(size, child), base), VecElement(index)) =>
        require(index >= 0 && index < size, "vector field index out of range")
        (child, Math.addExact(base, Math.multiplyExact(index, width(child))))
      case _ => throw new IllegalArgumentException("field path does not match entry layout")
    }
    (selected, offset, width(selected))
  }

  def path(steps: Vector[FieldOf.Step]): Vector[FieldStep] = steps.map {
    case FieldOf.Field(name) => BundleField(name)
    case FieldOf.Index(index) => VecElement(index)
  }

  def mask(width: Int): BigInt = { require(width > 0); (BigInt(1) << width) - 1 }
}
