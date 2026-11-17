// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import upickle.default.{ReadWriter, macroRW, read, write}

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
