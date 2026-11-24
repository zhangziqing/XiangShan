// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

object StorageFunctionQueries {
  def name(storage: StorageId, op: OpId): String = s"intention_storage_query_${storage.value}_${op.value}"
  def cpp(ir: StorageIR): String = ir.operations.collect {
    case op: ForeachOp => emit(ir, op.id, op.transform, Vector.empty, op.results, None)
    case op: CamQueryOp => emit(ir, op.id, op.predicate, op.params, op.results, Some(op.query))
  }.mkString("\n")
  private def emit(
    ir: StorageIR, op: OpId, function: FunctionId, params: Vector[Operand],
    results: Vector[ValueId], query: Option[CamQuery]
  ): String = {
    val types = ir.values.map(v => v.id -> v.tpe).toMap
    val packing = StorageQueryPacking(results.map(r => StorageLayout.width(types(r))))
    val args = params.indices.map(i => s", unsigned long long parameter_$i").mkString +
      (0 until packing.words).map(i => s", unsigned long long *word_$i").mkString
    val parameters = params.indices.map(i => s", parameter_$i").mkString
    val method = query match {
      case None => "foreach"
      case Some(AnyMatch) => "camAny"
      case Some(HitVector) => "camHitMask"
      case Some(HitIndices) => "camHitIdx"
      case Some(ReadMatches(n)) => s"camRead<$n>"
    }
    val values = if (query.contains(HitVector)) {
      (0 until packing.words).map(i => s"*word_$i = values[$i];").mkString("\n")
    } else results.indices.map { i =>
      val value = query match {
        case Some(_: ReadMatches) => if (i % 2 == 0) s"values.values[${i/2}]" else s"values.hits[${i/2}]"
        case Some(AnyMatch) => "values"
        case _ => s"values[$i]"
      }
      val field = packing.fields(i)
      val mask = s"UINT64_C(0x${StorageLayout.mask(field.width).toString(16)})"
      s"*word_${field.word} |= (static_cast<std::uint64_t>($value) & $mask) << ${field.shift};"
    }.mkString("\n")
    val clear = (0 until packing.words).map(i => s"*word_$i = 0;").mkString("\n")
    val overflow = query match { case Some(_: ReadMatches) => "values.overflow"; case _ => "0" }
    s"""extern "C" void ${name(ir.id, op)}(svBit epoch$args, svBit *overflow) {
       |  const auto values = storage_${ir.id.value}[epoch != 0].$method(${StorageFunctionCpp.batchName(function)}{}$parameters);
       |  $clear
       |  $values
       |  *overflow = $overflow;
       |}""".stripMargin
  }
}
