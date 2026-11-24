// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import firrtl.AnnotationSeq
import firrtl.annotations.TargetToken
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget}
import firrtl.options.Unserializable
import firrtl.options.{Dependency, Phase}
import firrtl.stage.FirrtlCircuitAnnotation
import scala.collection.mutable.ArrayBuffer
import upickle.default.{ReadWriter, macroRW, read, write}
import utils.QueueModel.FieldOf

final case class StorageId(value: Long)
final case class OpId(value: Int)
final case class ValueId(value: Int)
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
/** Reset semantics of the executable model, with the source signal's identity.
  * A clock-sampled model uses synchronous=true even for an AsyncReset source;
  * this flag describes model timing rather than the source signal's reset type.
  */
final case class ResetSpec(signal: SignalRef, synchronous: Boolean, activeHigh: Boolean)
sealed trait InitialState
case object UnspecifiedInitialState extends InitialState
final case class ConstantInitialState(bits: Vector[BigInt]) extends InitialState
final case class MaskedInitialState(bits: Vector[BigInt], masks: Vector[BigInt]) extends InitialState

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
  id: OpId, ordinal: Int, query: CamQuery, predicate: FunctionId,
  params: Vector[Operand], results: Vector[ValueId], source: SourceLoc
) extends StorageOp
final case class CamWriteOp(
  id: OpId, ordinal: Int, guard: Operand, predicate: FunctionId,
  params: Vector[Operand], fieldPath: Option[Vector[FieldStep]], value: Operand, source: SourceLoc
) extends StorageOp
final case class UpdateOp(
  id: OpId, ordinal: Int, guard: Operand, transform: FunctionId, source: SourceLoc
) extends StorageOp
final case class ForeachOp(
  id: OpId, ordinal: Int, transform: FunctionId, results: Vector[ValueId], source: SourceLoc
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
  operations: Vector[StorageOp],
  functions: Vector[StorageFunctionDef] = Vector.empty
)

object StorageIR {
  val CurrentVersion = 4
}

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
  implicit val maskedInitialRW: ReadWriter[MaskedInitialState] = macroRW
  implicit val initialRW: ReadWriter[InitialState] = ReadWriter.merge(unspecifiedRW, initialBitsRW, maskedInitialRW)
  implicit val hitVecRW: ReadWriter[HitVector.type] = macroRW
  implicit val hitIdxRW: ReadWriter[HitIndices.type] = macroRW
  implicit val matchesRW: ReadWriter[ReadMatches] = macroRW
  implicit val queryRW: ReadWriter[CamQuery] = ReadWriter.merge(hitVecRW, hitIdxRW, matchesRW)
  implicit val readRW: ReadWriter[ReadOp] = macroRW
  implicit val writeRW: ReadWriter[WriteOp] = macroRW
  implicit val writeFieldRW: ReadWriter[WriteFieldOp] = macroRW
  implicit val functionIdRW: ReadWriter[FunctionId] = macroRW
  implicit val camQueryRW: ReadWriter[CamQueryOp] = macroRW
  implicit val camWriteRW: ReadWriter[CamWriteOp] = macroRW
  implicit val updateRW: ReadWriter[UpdateOp] = macroRW
  implicit val foreachRW: ReadWriter[ForeachOp] = macroRW
  implicit val opRW: ReadWriter[StorageOp] =
    ReadWriter.merge(readRW, writeRW, writeFieldRW, camQueryRW, camWriteRW, updateRW, foreachRW)
  implicit val functionDefRW: ReadWriter[StorageFunctionDef] = macroRW
  implicit val irRW: ReadWriter[StorageIR] = macroRW

  def encode(ir: StorageIR): String = { StorageIRValidation.validate(ir); write(ir, indent = 2) }
  def decode(json: String): StorageIR = {
    val data = ujson.read(json)
    require(data.obj.get("expressions").forall(_.arr.isEmpty),
      "callback expression lowering is not implemented")
    val ir = read[StorageIR](data)
    StorageIRValidation.validate(ir)
    ir
  }
}

object StorageIRValidation {
  def validate(ir: StorageIR): Unit = {
    require(ir.schemaVersion >= 1 && ir.schemaVersion <= StorageIR.CurrentVersion, "unsupported storage IR schema")
    require(ir.schemaVersion >= 2 || ir.functions.isEmpty, "schema 1 cannot contain functions")
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
      case MaskedInitialState(values, masks) =>
        require(ir.schemaVersion >= 3, "masked reset requires schema 3")
        require(values.size == ir.entries && masks.size == ir.entries, "reset must describe every entry")
        values.zip(masks).foreach { case (value, mask) =>
          bits(value, ir.entry.shape); bits(mask, ir.entry.shape)
          require((value & ~mask) == 0, "initial bits outside reset coverage")
        }
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
    require(ir.functions.map(_.id).distinct.size == ir.functions.size, "duplicate function ID")
    ir.functions.foreach { f =>
      require(f.id.value >= 0 && f.module.nonEmpty && f.chirrtl.nonEmpty,
        "invalid function identity or body")
      require(f.inputs.nonEmpty && f.inputClasses.size == f.inputs.size && f.inputClasses.forall(_.nonEmpty) && f.outputClass.nonEmpty,
        "invalid function capture metadata")
      require(f.source.file.nonEmpty && f.source.line > 0 && f.source.column.forall(_ > 0), "invalid function source")
      (f.inputs :+ f.output).foreach(l => require(l == StorageLayout(l.shape), "invalid function layout"))
    }
    val functions = ir.functions.map(f => f.id -> f).toMap
    def function(id: FunctionId, params: Vector[Operand]): StorageFunctionDef = {
      val f = functions.getOrElse(id, throw new IllegalArgumentException(s"unresolved function $id"))
      require(f.inputs.map(_.shape) == Vector(ir.entry.shape) ++ params.map(operand), "function signature mismatch")
      f
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
          require(function(p, params).output.shape == UIntType(1))
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
          guard(g); require(function(p, params).output.shape == UIntType(1)); require(operand(v) == field(path))
        case UpdateOp(_, _, g, transform, _) => guard(g); require(function(transform, Vector.empty).output.shape == ir.entry.shape)
        case ForeachOp(_, _, transform, rs, _) =>
          require(rs.size == ir.entries); rs.foreach(result(_, function(transform, Vector.empty).output.shape))
      }
    }
    val resultOwners = ir.operations.flatMap {
      case op: ReadOp => Vector(op.result -> op.id)
      case op: ForeachOp => op.results.map(_ -> op.id)
      case op: CamQueryOp => op.results.map(_ -> op.id)
      case _ => Vector.empty
    }.toMap
    ir.values.foreach { v => v.binding match {
      case ReadResultValue(op, _) =>
        require(results(v.id) && resultOwners.get(v.id).contains(op), "orphaned or mismatched result anchor")
      case _ => ()
    } }
  }

  /** Explicit capability boundary of the first executable bridge. */
  def requireDPI(ir: StorageIR): Unit = {
    validate(ir)
    require(ir.entry.totalWidth <= 64, "DPI entry width must be at most 64 bits")
    require(ir.reset.synchronous && ir.reset.activeHigh, "DPI requires clock-sampled active-high reset semantics")
    ir.functions.foreach(StorageFunctionCpp.function)
    val types = ir.values.map(v => v.id -> v.tpe).toMap
    ir.operations.foreach {
      case ReadOp(_, _, i, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case WriteOp(_, _, _, i, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case WriteFieldOp(_, _, _, i, _, _, _) => require(StorageLayout.width(types(i.value)) <= 64, "index exceeds 64 bits")
      case _ => ()
    }
  }
}

/** Stage consumes these exact capture anchors before calling firtool. */
private[IntentionModel] final case class StorageWiringAnnotation(
  module: String,
  wires: Vector[String],
  instances: Vector[String],
  defaults: Vector[ReferenceTarget],
  connections: Vector[(ReferenceTarget, ReferenceTarget)]
) extends NoTargetAnnotation with Unserializable

private[IntentionModel] final case class StoragePendingIRAnnotation(finish: () => Unit)
  extends NoTargetAnnotation with Unserializable

/** Explicit per-elaboration ownership: IDs restart in a new session. */
final class StorageIRSession {
  private var nextId = 0L
  private val artifacts = ArrayBuffer.empty[StorageIR]
  private var nextFunctionId = 0L
  private val functionArtifacts = ArrayBuffer.empty[StorageFunctionDef]
  private[IntentionModel] def allocateFunction(): FunctionId = {
    val id = FunctionId(nextFunctionId)
    nextFunctionId = Math.addExact(nextFunctionId, 1L)
    id
  }
  private[IntentionModel] def recordFunction(f: StorageFunctionDef): Unit = {
    require(!functionArtifacts.exists(_.id == f.id), "duplicate function ID")
    functionArtifacts += f
  }
  def functions: Vector[StorageFunctionDef] = functionArtifacts.toVector
  private[IntentionModel] def allocate(): StorageId = {
    val id = StorageId(nextId)
    nextId = Math.addExact(nextId, 1L)
    id
  }
  private[IntentionModel] def record(ir: StorageIR): Unit = {
    require(!artifacts.exists(_.id == ir.id), "duplicate storage instance ID")
    artifacts += ir
  }
  def models: Vector[StorageIR] = artifacts.toVector
}

private[IntentionModel] final case class StorageWritePort(
  guard: ValueId, index: ValueId, value: ValueId
)

/** Data identities live only here and are released after Stage sealing. */
final class StorageIRBuilder(
  val id: StorageId, val entries: Int, val layout: EntryLayout, val initial: InitialState
) {
  private final case class Captured(id: ValueId, data: Data, tpe: StorageType, read: Option[OpId])
  private val captured = ArrayBuffer.empty[Captured]
  private val ops = ArrayBuffer.empty[StorageOp]
  private val functionIds = ArrayBuffer.empty[FunctionId]

  private[IntentionModel] def function[T <: Data, R <: Data](
    makeInput: () => T, makeParam: Option[() => Data],
    callback: (T, Option[Data]) => R, normalize: Option[() => R], source: SourceLoc
  )(implicit session: StorageIRSession): (FunctionId, R) = {
    open()
    val fid = session.allocateFunction()
    val module = chisel3.Module(new StorageFunctionModule(fid, makeInput, makeParam,
      callback, normalize, source, session))
    module.arguments.foreach(_ := 0.U)
    functionIds += fid
    (fid, module.resultType)
  }
  private var closed = false
  private var sealedIR: Option[StorageIR] = None
  private def open(): Unit = require(!closed && sealedIR.isEmpty, "storage capture is closed")
  private[IntentionModel] def nextOp: OpId = { open(); OpId(ops.size) }
  private[IntentionModel] def capture(data: Data, tpe: StorageType, read: Option[OpId] = None): ValueId = {
    open()
    require(data.getWidth == StorageLayout.width(tpe), "captured value width mismatch")
    val value = ValueId(captured.size)
    captured += Captured(value, data, tpe, read)
    value
  }
  private[IntentionModel] def read(index: ValueId, data: Data, source: SourceLoc): Unit = {
    val op = nextOp
    val result = capture(data, layout.shape, Some(op))
    ops += ReadOp(op, ops.size, Operand(index), result, source)
  }
  private[IntentionModel] def write(
    guard: ValueId, index: ValueId, value: ValueId, path: Option[Vector[FieldStep]], source: SourceLoc
  ): Unit = {
    val op = nextOp
    ops += (path match {
      case Some(p) => WriteFieldOp(op, ops.size, Operand(guard), Operand(index), p, Operand(value), source)
      case None => WriteOp(op, ops.size, Operand(guard), Operand(index), Operand(value), source)
    })
  }
  private[IntentionModel] def operations: Vector[StorageOp] = ops.toVector
  private[IntentionModel] def update(guard: ValueId, function: FunctionId, source: SourceLoc): Unit = {
    val op = nextOp
    ops += UpdateOp(op, ops.size, Operand(guard), function, source)
  }
  private[IntentionModel] def foreach(
    function: FunctionId, results: Vector[Data], tpe: StorageType, source: SourceLoc
  ): Unit = {
    val op = nextOp
    val ids = results.map(capture(_, tpe, Some(op)))
    ops += ForeachOp(op, ops.size, function, ids, source)
  }
  private[IntentionModel] def camQuery(
    query: CamQuery, function: FunctionId, params: Vector[ValueId],
    results: Vector[(Data, StorageType)], source: SourceLoc
  ): Unit = {
    val op = nextOp
    val ids = results.map { case (data, tpe) => capture(data, tpe, Some(op)) }
    ops += CamQueryOp(op, ops.size, query, function, params.map(Operand), ids, source)
  }
  private[IntentionModel] def camWrite(
    function: FunctionId, guard: ValueId, params: Vector[ValueId], path: Option[Vector[FieldStep]],
    value: ValueId, source: SourceLoc
  ): Unit = {
    val op = nextOp
    ops += CamWriteOp(op, ops.size, Operand(guard), function, params.map(Operand), path, Operand(value), source)
  }
  private[IntentionModel] def writes: Vector[StorageWritePort] = ops.collect {
    case WriteOp(_, _, guard, index, value, _) => StorageWritePort(guard.value, index.value, value.value)
    case WriteFieldOp(_, _, guard, index, _, value, _) => StorageWritePort(guard.value, index.value, value.value)
  }.toVector
  private[IntentionModel] def data(id: ValueId): Data = captured(id.value).data
  private[IntentionModel] def closeCapture(): Unit = { open(); closed = true }

  /** Called explicitly by the Stage annotation conversion after elaboration. */
  def seal(
    instancePath: String, clock: SignalRef, reset: ResetSpec, resolve: Data => SignalRef,
    definitions: Vector[StorageFunctionDef] = Vector.empty
  ): StorageIR = sealedIR.getOrElse {
    require(closed, "close capture before sealing")
    val values = captured.map { c =>
      val binding = c.read match {
        case Some(op) => ReadResultValue(op, resolve(c.data))
        case None => c.data.litOption match {
          case Some(bits) => ConstantValue(bits & StorageLayout.mask(StorageLayout.width(c.tpe)))
          case None => SignalValue(resolve(c.data))
        }
      }
      StorageValue(c.id, c.tpe, binding)
    }.toVector
    val byId = definitions.map(f => f.id -> f).toMap
    val functions = functionIds.map(fid => byId.getOrElse(fid,
      throw new IllegalArgumentException(s"function $fid has not been lowered"))).toVector
    val ir = StorageIR(StorageIR.CurrentVersion, id, Some(instancePath), entries, layout, clock, reset, initial,
      values, ops.toVector, functions)
    StorageIRValidation.requireDPI(ir)
    sealedIR = Some(ir)
    captured.clear()
    ops.clear()
    functionIds.clear()
    ir
  }
}

/** Lift only explicitly annotated capture declarations, preserving guarded connects.
  * This runs on elaborated FIRRTL, before CIRCT; it never recognizes storage patterns.
  */
class StorageCapturePhase extends Phase {
  import firrtl.ir.{UIntType => FirrtlUIntType, _}
  import xiangshan.transforms.Helpers._

  override def prerequisites = Seq(Dependency[chisel3.stage.phases.Convert])
  override def optionalPrerequisiteOf = Seq(Dependency[circt.stage.phases.CIRCT])
  override def invalidates(a: Phase): Boolean = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val wiring = annotations.collect { case a: StorageWiringAnnotation => a }
    if (wiring.isEmpty) return annotations
    val byModule = wiring.groupBy(_.module)
    def expression(target: ReferenceTarget, owner: String): Expression = {
      require(target.module == owner && target.path.size <= 1, "nonlocal storage capture target")
      val root: Expression = target.path.headOption match {
        case Some((instance, _)) => SubField(Reference(instance.value), target.ref)
        case None => Reference(target.ref)
      }
      target.component.foldLeft(root) {
        case (e, TargetToken.Field(name)) => SubField(e, name)
        case (e, TargetToken.Index(index)) => SubIndex(e, index, UnknownType)
        case _ => throw new IllegalArgumentException("unsupported storage target component")
      }
    }
    val circuits = annotations.collect { case c: FirrtlCircuitAnnotation => c }
    require(circuits.size == 1, "storage capture requires one elaborated circuit")
    val seen = scala.collection.mutable.Set.empty[String]
    def module(m: DefModule): DefModule = m match {
      case mod: Module if byModule.contains(mod.name) =>
        seen += mod.name
        val captures = byModule(mod.name)
        val wires = captures.flatMap(_.wires).toSet
        val instances = captures.flatMap(_.instances).toSet
        val declarations = scala.collection.mutable.ArrayBuffer.empty[Statement]
        val foundWires = scala.collection.mutable.Set.empty[String]
        val foundInstances = scala.collection.mutable.Set.empty[String]
        def lift(s: Statement): Statement = s match {
          case w: DefWire if wires(w.name) =>
            require(w.tpe.isInstanceOf[FirrtlUIntType], "storage capture wires must be packed UInt")
            declarations += w; foundWires += w.name; EmptyStmt
          case i: DefInstance if instances(i.name) =>
            declarations += i; foundInstances += i.name; EmptyStmt
          case other => other.mapStmt(lift)
        }
        val body = lift(mod.body)
        require(foundWires.toSet == wires && foundInstances.toSet == instances, "missing storage capture anchors")
        val defaults = foundWires.toVector.sorted.map(name => ReferenceTarget(
          circuits.head.circuit.main, mod.name, Seq.empty, name, Seq.empty)) ++ captures.flatMap(_.defaults)
        val zero = defaults.map(t => Connect(NoInfo, expression(t, mod.name), UIntLiteral(0, IntWidth(1))))
        val connections = captures.flatMap(_.connections).map { case (sink, source) =>
          Connect(NoInfo, expression(sink, mod.name), expression(source, mod.name))
        }
        mod.copy(body = Block(declarations.toVector ++ zero ++ Vector(body) ++ connections))
      case other => other
    }
    val updated = circuits.head.copy(circuit = circuits.head.circuit.mapModule(module))
    require(seen.toSet == byModule.keySet, "unresolved storage module anchor")
    annotations.flatMap {
      case _: FirrtlCircuitAnnotation => Seq(updated)
      case _: StorageWiringAnnotation => Seq.empty
      case other => Seq(other)
    }
  }
}
