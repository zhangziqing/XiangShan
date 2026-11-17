// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

final case class StorageCycleField(name: String, width: Int, offset: Int) {
  def end: Int = offset + width
  def word: Int = offset / 64
  def shift: Int = offset % 64
}

/** Fixed operand layout shared by the SV sampler and generated C++ decoder.
  * Fields never cross a 64-bit DPI word; padding is always zero.
  */
final case class StorageCyclePacking(writeWidths: Vector[(Int, Int)]) {
  val fields: Vector[StorageCycleField] = {
    val widths = Vector("reset" -> 1) ++ writeWidths.zipWithIndex.flatMap { case ((index, value), i) =>
      Vector(s"guard_$i" -> 1, s"index_$i" -> index, s"value_$i" -> value)
    }
    var next = 0
    widths.map { case (name, width) =>
      require(width > 0 && width <= 64, "cycle operand width must be in 1..64")
      if (next % 64 + width > 64) next += 64 - next % 64
      val field = StorageCycleField(name, width, next)
      next = field.end
      field
    }
  }
  val totalWidth: Int = fields.last.end
  val words: Int = (totalWidth + 63) / 64
  def field(name: String): StorageCycleField = fields.find(_.name == name).get
}

object StorageCyclePacking {
  def fromIR(ir: StorageIR): StorageCyclePacking = {
    val types = ir.values.map(v => v.id -> v.tpe).toMap
    def widths(index: Operand, value: Operand): (Int, Int) =
      StorageLayout.width(types(index.value)) -> StorageLayout.width(types(value.value))
    StorageCyclePacking(ir.operations.collect {
      case WriteOp(_, _, _, index, value, _) => widths(index, value)
      case WriteFieldOp(_, _, _, index, _, value, _) => widths(index, value)
    })
  }
}
