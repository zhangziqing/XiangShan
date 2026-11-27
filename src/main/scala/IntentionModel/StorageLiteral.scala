// SPDX-License-Identifier: MulanPSL-2.0
package chisel3

import chisel3.internal.LitBinding
import IntentionModel._

/** Chisel 6.7 literal bindings distinguish omitted aggregate members from hardware.
  * Keep this small compatibility boundary separate from descriptor semantics.
  */
object StorageLiteral {
  def initial(values: Seq[Data], layout: EntryLayout): InitialState = {
    val words = values.map { value =>
      require(value.topBinding.isInstanceOf[LitBinding],
        "ModelStorage initialization requires a scalar or aggregate literal")
      layout.leaves.foldLeft((BigInt(0), BigInt(0))) { case ((bits, mask), leaf) =>
        val member = leaf.path.foldLeft(value) {
          case (record: Record, BundleField(name)) => record.elements(name)
          case (vector: Vec[_], VecElement(index)) => vector(index)
          case _ => throw new IllegalArgumentException("initializer shape differs from storage entry")
        }
        member.litOption match {
          case Some(literal) =>
            val fieldMask = StorageLayout.mask(leaf.width)
            (bits | ((literal & fieldMask) << leaf.offset), mask | (fieldMask << leaf.offset))
          case None => (bits, mask)
        }
      }
    }.toVector
    if (words.forall(_._2 == StorageLayout.mask(layout.totalWidth)))
      ConstantInitialState(words.map(_._1))
    else MaskedInitialState(words.map(_._1), words.map(_._2))
  }
}
