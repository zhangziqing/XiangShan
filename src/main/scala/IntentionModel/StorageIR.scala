// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

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
