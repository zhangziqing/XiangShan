// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

object StorageIRValidation {
  def validate(ir: StorageIR): Unit = {
    require(ir.schemaVersion == 1, "unsupported storage IR schema")
    require(ir.id.value >= 0 && ir.entries > 0, "invalid storage identity or capacity")
    require(ir.entry == StorageLayout(ir.entry.shape), "inconsistent entry layout or packing")
    require(ir.clock.target.nonEmpty && ir.clock.tpe == UIntType(1), "invalid clock reference")
    require(ir.reset.signal.target.nonEmpty && ir.reset.signal.tpe == UIntType(1), "invalid reset reference")
    require(ir.instancePath.forall(_.nonEmpty), "empty instance path")
    def bits(value: BigInt, tpe: StorageType): Unit =
      require(value >= 0 && value <= StorageLayout.mask(StorageLayout.width(tpe)), "constant out of range")
    ir.initial match {
      case ConstantInitialState(values) =>
        require(values.size == ir.entries, "initial state must describe every entry")
        values.foreach(bits(_, ir.entry.shape))
      case UnspecifiedInitialState => ()
    }
    require(ir.values.map(_.id).distinct.size == ir.values.size, "duplicate value ID")
    require(ir.values.forall(_.id.value >= 0), "negative value ID")
    val values = ir.values.map(v => v.id -> v).toMap
    def tpe(id: ValueId): StorageType = values.getOrElse(id,
      throw new IllegalArgumentException(s"unresolved value $id")).tpe
    def operand(op: Operand): StorageType = tpe(op.value)
    def signal(ref: SignalRef, expected: StorageType): Unit =
      require(ref.target.nonEmpty && (ref.tpe == expected ||
        ref.tpe == UIntType(StorageLayout.width(expected))), "invalid signal binding")
    ir.values.foreach { v =>
      StorageLayout.width(v.tpe)
      v.binding match {
        case ConstantValue(b) => bits(b, v.tpe)
        case SignalValue(s) => signal(s, v.tpe)
        case ReadResultValue(_, s) => signal(s, v.tpe)
      }
    }
    require(ir.expressions.map(_.id).distinct.size == ir.expressions.size, "duplicate expression ID")
    val expressions = scala.collection.mutable.Map.empty[ExprId, StorageType]
    def expr(id: ExprId): StorageType = expressions.getOrElse(id,
      throw new IllegalArgumentException(s"expression dependency missing or not topological: $id"))
    ir.expressions.foreach { e =>
      require(e.id.value >= 0, "negative expression ID")
      StorageLayout.width(e.resultType)
      e match {
        case FieldRef(_, path, result) => require(StorageLayout.field(ir.entry, path)._1 == result)
        case ParamRef(_, value, result) => require(tpe(value) == result)
        case Literal(_, b, result) => bits(b, result)
        case Unary(_, _, arg, result) =>
          require(expr(arg) == result && (result.isInstanceOf[UIntType] || result.isInstanceOf[SIntType]))
        case Binary(_, op, lhs, rhs, result) =>
          require(expr(lhs) == expr(rhs), "binary operand type mismatch")
          require(expr(lhs).isInstanceOf[UIntType] || expr(lhs).isInstanceOf[SIntType])
          require(result == (op match { case Equal | LessThan => UIntType(1); case _ => expr(lhs) }))
        case Mux(_, cond, t, f, result) =>
          require(expr(cond) == UIntType(1) && expr(t) == result && expr(f) == result)
      }
      expressions(e.id) = e.resultType
    }
    require(ir.operations.map(_.id).distinct.size == ir.operations.size, "duplicate operation ID")
    require(ir.operations.map(_.ordinal) == ir.operations.indices.toVector, "non-contiguous operation order")
    val results = scala.collection.mutable.Set.empty[ValueId]
    def result(id: ValueId, expected: StorageType): Unit = {
      require(tpe(id) == expected && results.add(id), "invalid or multiply defined operation result")
    }
    def guard(g: Operand): Unit = require(operand(g) == UIntType(1), "guard must be one bit")
    def index(i: Operand): Unit = require(operand(i).isInstanceOf[UIntType], "index must be unsigned")
    def field(path: Option[Vector[FieldStep]]): StorageType = path match {
      case Some(p) => require(p.nonEmpty, "field writes must select a member"); StorageLayout.field(ir.entry, p)._1
      case None => ir.entry.shape
    }
    ir.operations.foreach { op =>
      require(op.id.value >= 0 && op.source.file.nonEmpty && op.source.line > 0, "invalid operation metadata")
      require(op.source.column.forall(_ > 0), "invalid source column")
      op match {
        case ReadOp(id, _, i, r, _) =>
          index(i); result(r, ir.entry.shape)
          require(values(r).binding match { case ReadResultValue(`id`, _) => true; case _ => false },
            "read result anchor mismatch")
        case WriteOp(_, _, g, i, v, _) => guard(g); index(i); require(operand(v) == ir.entry.shape)
        case WriteFieldOp(_, _, g, i, p, v, _) => guard(g); index(i); require(operand(v) == field(Some(p)))
        case CamQueryOp(_, _, query, p, params, rs, _) =>
          require(expr(p) == UIntType(1)); params.foreach(operand)
          query match {
            case HitVector => require(rs.size == ir.entries); rs.foreach(result(_, UIntType(1)))
            case HitIndices =>
              require(rs.size == ir.entries)
              rs.foreach(result(_, UIntType(math.max(1, BigInt(ir.entries).bitLength))))
            case ReadMatches(n) =>
              require(n > 0 && n <= ir.entries && rs.size == n * 2)
              rs.grouped(2).foreach(pair => { result(pair(0), ir.entry.shape); result(pair(1), UIntType(1)) })
          }
        case CamWriteOp(_, _, g, p, params, path, v, _) =>
          guard(g); require(expr(p) == UIntType(1)); params.foreach(operand); require(operand(v) == field(path))
        case UpdateOp(_, _, g, transform, _) => guard(g); require(expr(transform) == ir.entry.shape)
        case ForeachOp(_, _, transform, rs, _) =>
          require(rs.size == ir.entries); rs.foreach(result(_, expr(transform)))
      }
    }
    ir.values.foreach { v => v.binding match {
      case ReadResultValue(_, _) => require(results(v.id), "orphaned read result")
      case _ => ()
    } }
  }

  /** Explicit capability boundary of the first executable bridge. */
  def requireDPI(ir: StorageIR): Unit = {
    validate(ir)
    require(ir.entry.totalWidth <= 64, "DPI entry width must be at most 64 bits")
    require(ir.reset.synchronous && ir.reset.activeHigh, "DPI requires synchronous active-high reset")
    require(ir.expressions.isEmpty, "callback expression lowering is not implemented")
    ir.operations.foreach {
      case _: ReadOp | _: WriteOp | _: WriteFieldOp => ()
      case _ => throw new IllegalArgumentException("CAM/update/foreach DPI lowering is not implemented")
    }
    val types = ir.values.map(v => v.id -> v.tpe).toMap
    ir.operations.foreach {
      case ReadOp(_, _, i, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case WriteOp(_, _, _, i, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case WriteFieldOp(_, _, _, i, _, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case _ => ()
    }
  }
}
