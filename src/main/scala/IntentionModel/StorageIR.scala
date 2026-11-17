// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3.{Bundle, Data, Record, SInt, UInt, Vec}
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.options.Unserializable
import upickle.default.{ReadWriter, macroRW, read, write}
import utils.QueueModel.FieldOf

final case class StorageId(value: Long)
final case class OpId(value: Int)
final case class ValueId(value: Int)
final case class ExprId(value: Int)
final case class SourceLoc(file: String, line: Int, column: Option[Int] = None)

sealed trait StorageType
final case class UIntType(width: Int) extends StorageType
final case class SIntType(width: Int) extends StorageType
/** Fields follow Chisel getElements order, from least to most significant. */
final case class BundleType(fields: Vector[(String, StorageType)]) extends StorageType
final case class VecType(size: Int, element: StorageType) extends StorageType

sealed trait FieldStep
final case class BundleField(name: String) extends FieldStep
final case class VecElement(index: Int) extends FieldStep
final case class LeafLayout(path: Vector[FieldStep], offset: Int, width: Int, signed: Boolean)
final case class EntryLayout(
  shape: StorageType,
  totalWidth: Int,
  leaves: Vector[LeafLayout],
  packing: String
)

/** A source-level, elaborated FIRRTL target, before lowering or renaming. */
final case class SignalRef(target: String, tpe: StorageType)
sealed trait ValueBinding
final case class SignalValue(signal: SignalRef) extends ValueBinding
final case class ConstantValue(bits: BigInt) extends ValueBinding
final case class ReadResultValue(op: OpId, signal: SignalRef) extends ValueBinding
final case class StorageValue(id: ValueId, tpe: StorageType, binding: ValueBinding)
final case class Operand(value: ValueId)
final case class ResetSpec(signal: SignalRef, synchronous: Boolean, activeHigh: Boolean)
sealed trait InitialState
case object UnspecifiedInitialState extends InitialState
final case class ConstantInitialState(bits: Vector[BigInt]) extends InitialState

sealed trait UnaryOp
case object BitNot extends UnaryOp
case object Negate extends UnaryOp
sealed trait BinaryOp
case object BitAnd extends BinaryOp
case object BitOr extends BinaryOp
case object BitXor extends BinaryOp
case object Add extends BinaryOp
case object Subtract extends BinaryOp
case object Equal extends BinaryOp
case object LessThan extends BinaryOp

/** Reserved callback graph. Arithmetic wraps to resultType's explicit width. */
sealed trait ExprNode { def id: ExprId; def resultType: StorageType }
final case class FieldRef(id: ExprId, path: Vector[FieldStep], resultType: StorageType) extends ExprNode
final case class ParamRef(id: ExprId, value: ValueId, resultType: StorageType) extends ExprNode
final case class Literal(id: ExprId, bits: BigInt, resultType: StorageType) extends ExprNode
final case class Unary(id: ExprId, op: UnaryOp, arg: ExprId, resultType: StorageType) extends ExprNode
final case class Binary(
  id: ExprId, op: BinaryOp, lhs: ExprId, rhs: ExprId, resultType: StorageType
) extends ExprNode
final case class Mux(
  id: ExprId, cond: ExprId, t: ExprId, f: ExprId, resultType: StorageType
) extends ExprNode

sealed trait StorageOp { def id: OpId; def ordinal: Int; def source: SourceLoc }
final case class ReadOp(
  id: OpId, ordinal: Int, index: Operand, result: ValueId, source: SourceLoc
) extends StorageOp
final case class WriteOp(
  id: OpId, ordinal: Int, guard: Operand, index: Operand, value: Operand, source: SourceLoc
) extends StorageOp
final case class WriteFieldOp(
  id: OpId, ordinal: Int, guard: Operand, index: Operand,
  fieldPath: Vector[FieldStep], value: Operand, source: SourceLoc
) extends StorageOp

sealed trait CamQuery
case object HitVector extends CamQuery
case object HitIndices extends CamQuery
final case class ReadMatches(maxHits: Int) extends CamQuery
final case class CamQueryOp(
  id: OpId, ordinal: Int, query: CamQuery, predicate: ExprId,
  params: Vector[Operand], results: Vector[ValueId], source: SourceLoc
) extends StorageOp
final case class CamWriteOp(
  id: OpId, ordinal: Int, guard: Operand, predicate: ExprId,
  params: Vector[Operand], fieldPath: Option[Vector[FieldStep]], value: Operand, source: SourceLoc
) extends StorageOp
final case class UpdateOp(
  id: OpId, ordinal: Int, guard: Operand, transform: ExprId, source: SourceLoc
) extends StorageOp
final case class ForeachOp(
  id: OpId, ordinal: Int, transform: ExprId, results: Vector[ValueId], source: SourceLoc
) extends StorageOp

/** Immutable boundary IR: no Chisel objects, callbacks, or runtime state. */
final case class StorageIR(
  schemaVersion: Int,
  id: StorageId,
  instancePath: Option[String],
  entries: Int,
  entry: EntryLayout,
  clock: SignalRef,
  reset: ResetSpec,
  initial: InitialState,
  values: Vector[StorageValue],
  expressions: Vector[ExprNode],
  operations: Vector[StorageOp]
)

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

/** Versioned JSON; BigInt fields use decimal strings, without Double rounding. */
object StorageIRCodec {
  implicit val bigIntRW: ReadWriter[BigInt] = upickle.default.readwriter[String].bimap(_.toString, BigInt(_))
  implicit val storageIdRW: ReadWriter[StorageId] = macroRW
  implicit val opIdRW: ReadWriter[OpId] = macroRW
  implicit val valueIdRW: ReadWriter[ValueId] = macroRW
  implicit val exprIdRW: ReadWriter[ExprId] = macroRW
  implicit val sourceRW: ReadWriter[SourceLoc] = macroRW
  implicit val uintRW: ReadWriter[UIntType] = macroRW
  implicit val sintRW: ReadWriter[SIntType] = macroRW
  implicit lazy val bundleRW: ReadWriter[BundleType] = macroRW
  implicit lazy val vecRW: ReadWriter[VecType] = macroRW
  implicit lazy val typeRW: ReadWriter[StorageType] = ReadWriter.merge(uintRW, sintRW, bundleRW, vecRW)
  implicit val fieldRW: ReadWriter[BundleField] = macroRW
  implicit val elementRW: ReadWriter[VecElement] = macroRW
  implicit val stepRW: ReadWriter[FieldStep] = ReadWriter.merge(fieldRW, elementRW)
  implicit val leafRW: ReadWriter[LeafLayout] = macroRW
  implicit val layoutRW: ReadWriter[EntryLayout] = macroRW
  implicit val signalRW: ReadWriter[SignalRef] = macroRW
  implicit val signalValueRW: ReadWriter[SignalValue] = macroRW
  implicit val constantRW: ReadWriter[ConstantValue] = macroRW
  implicit val readResultRW: ReadWriter[ReadResultValue] = macroRW
  implicit val bindingRW: ReadWriter[ValueBinding] = ReadWriter.merge(signalValueRW, constantRW, readResultRW)
  implicit val valueRW: ReadWriter[StorageValue] = macroRW
  implicit val operandRW: ReadWriter[Operand] = macroRW
  implicit val resetRW: ReadWriter[ResetSpec] = macroRW
  implicit val unspecifiedRW: ReadWriter[UnspecifiedInitialState.type] = macroRW
  implicit val initialBitsRW: ReadWriter[ConstantInitialState] = macroRW
  implicit val initialRW: ReadWriter[InitialState] = ReadWriter.merge(unspecifiedRW, initialBitsRW)
  implicit val bitNotRW: ReadWriter[BitNot.type] = macroRW
  implicit val negateRW: ReadWriter[Negate.type] = macroRW
  implicit val unaryOpRW: ReadWriter[UnaryOp] = ReadWriter.merge(bitNotRW, negateRW)
  implicit val andRW: ReadWriter[BitAnd.type] = macroRW
  implicit val orRW: ReadWriter[BitOr.type] = macroRW
  implicit val xorRW: ReadWriter[BitXor.type] = macroRW
  implicit val addRW: ReadWriter[Add.type] = macroRW
  implicit val subRW: ReadWriter[Subtract.type] = macroRW
  implicit val eqRW: ReadWriter[Equal.type] = macroRW
  implicit val ltRW: ReadWriter[LessThan.type] = macroRW
  implicit val binaryOpRW: ReadWriter[BinaryOp] = ReadWriter.merge(andRW, orRW, xorRW, addRW, subRW, eqRW, ltRW)
  implicit val fieldRefRW: ReadWriter[FieldRef] = macroRW
  implicit val paramRW: ReadWriter[ParamRef] = macroRW
  implicit val literalRW: ReadWriter[Literal] = macroRW
  implicit val unaryRW: ReadWriter[Unary] = macroRW
  implicit val binaryRW: ReadWriter[Binary] = macroRW
  implicit val muxRW: ReadWriter[Mux] = macroRW
  implicit val exprRW: ReadWriter[ExprNode] =
    ReadWriter.merge(fieldRefRW, paramRW, literalRW, unaryRW, binaryRW, muxRW)
  implicit val hitVecRW: ReadWriter[HitVector.type] = macroRW
  implicit val hitIdxRW: ReadWriter[HitIndices.type] = macroRW
  implicit val matchesRW: ReadWriter[ReadMatches] = macroRW
  implicit val queryRW: ReadWriter[CamQuery] = ReadWriter.merge(hitVecRW, hitIdxRW, matchesRW)
  implicit val readRW: ReadWriter[ReadOp] = macroRW
  implicit val writeRW: ReadWriter[WriteOp] = macroRW
  implicit val writeFieldRW: ReadWriter[WriteFieldOp] = macroRW
  implicit val camQueryRW: ReadWriter[CamQueryOp] = macroRW
  implicit val camWriteRW: ReadWriter[CamWriteOp] = macroRW
  implicit val updateRW: ReadWriter[UpdateOp] = macroRW
  implicit val foreachRW: ReadWriter[ForeachOp] = macroRW
  implicit val opRW: ReadWriter[StorageOp] =
    ReadWriter.merge(readRW, writeRW, writeFieldRW, camQueryRW, camWriteRW, updateRW, foreachRW)
  implicit val irRW: ReadWriter[StorageIR] = macroRW

  def encode(ir: StorageIR): String = { StorageIRValidation.validate(ir); write(ir, indent = 2) }
  def decode(json: String): StorageIR = {
    val ir = read[StorageIR](json)
    StorageIRValidation.validate(ir)
    ir
  }
}

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

final case class StorageReadResultAnnotation(
  target: ReferenceTarget, storageId: Long, opId: Int, resultId: Int
) extends SingleTargetAnnotation[ReferenceTarget] with Unserializable {
  def duplicate(n: ReferenceTarget): StorageReadResultAnnotation = copy(target = n)
}

/** Stage consumes these exact capture anchors before calling firtool. */
private[IntentionModel] final case class StorageWiringAnnotation(
  module: String,
  wires: Vector[String],
  instances: Vector[String],
  defaults: Vector[ReferenceTarget],
  connections: Vector[(ReferenceTarget, ReferenceTarget)]
) extends NoTargetAnnotation with Unserializable

final case class StorageIRAnnotation(json: String) extends NoTargetAnnotation with Unserializable {
  def ir: StorageIR = StorageIRCodec.decode(json)
}
