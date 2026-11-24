// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.Data._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Dependency, Phase, Unserializable}
import firrtl.stage.FirrtlCircuitAnnotation

final case class FunctionId(value: Long)

/** Function bodies are compiler IR, not a second expression language. */
final case class StorageFunctionDef(
  id: FunctionId,
  module: String,
  source: SourceLoc,
  inputs: Vector[EntryLayout],
  output: EntryLayout,
  inputClasses: Vector[String],
  outputClass: String,
  chirrtl: String,
  firrtl: String,
  systemVerilog: String = ""
)

private[IntentionModel] final case class StorageFunctionCaptureAnnotation(
  id: FunctionId, module: String, source: SourceLoc,
  inputs: Vector[EntryLayout], output: EntryLayout,
  inputClasses: Vector[String], outputClass: String, session: StorageIRSession
) extends NoTargetAnnotation with Unserializable

/** Instantiated synchronously at the Storage API call. The callback executes once,
  * with its concrete Scala type, inside a clockless child module. No callback or
  * Data object survives in the sealed function definition.
  */
private[IntentionModel] class StorageFunctionModule[T <: Data, R <: Data](
  id: FunctionId, makeInput: () => T, makeParam: Option[() => Data],
  callback: (T, Option[Data]) => R, normalize: Option[() => R],
  source: SourceLoc, session: StorageIRSession
) extends RawModule {
  override def desiredName: String = s"StorageFunction_${id.value}"
  private val prototype = makeInput()
  private val parameter = makeParam.map(_())
  private val prototypes = Vector(prototype) ++ parameter.toVector
  private val layouts = prototypes.map(StorageLayout.fromData)
  require(layouts.forall(_.totalWidth <= 64), "function inputs must fit in 64 bits")
  val arguments: Vector[UInt] = layouts.zipWithIndex.map { case (layout, i) =>
    IO(Input(UInt(layout.totalWidth.W))).suggestName(s"argument_$i")
  }
  private val input = arguments.head.asTypeOf(prototype)
  private val param = parameter.map(p => arguments(1).asTypeOf(p))
  private val raw = callback(input.readOnly, param.map(_.readOnly))
  private val normalized = normalize.map { make =>
    val value = Wire(make())
    value := raw
    value
  }.getOrElse(raw)
  require(normalized.isWidthKnown, "function result must have explicit, known widths")
  val resultType: R = chiselTypeOf(normalized)
  private val resultLayout = StorageLayout.fromData(resultType)
  require(resultLayout.totalWidth <= 64, "function output must fit in 64 bits")
  val result = IO(Output(UInt(resultLayout.totalWidth.W)))
  result := normalized.asUInt
  annotate(new ChiselAnnotation {
    def toFirrtl = StorageFunctionCaptureAnnotation(id, StorageFunctionModule.this.name, source,
      layouts, resultLayout, prototypes.map(_.getClass.getName), resultType.getClass.getName, session)
  })
}

/** Capture is complete before this phase; conversion and sealing happen here. */
class StorageFunctionPhase extends Phase {
  override def prerequisites = Seq(Dependency[StorageCapturePhase])
  override def optionalPrerequisiteOf = Seq(Dependency[circt.stage.phases.CIRCT])
  override def invalidates(a: Phase): Boolean = false
  override def transform(annotations: firrtl.AnnotationSeq): firrtl.AnnotationSeq = {
    val functions = annotations.collect { case a: StorageFunctionCaptureAnnotation => a }
    val circuits = annotations.collect { case a: FirrtlCircuitAnnotation => a.circuit }
    require(circuits.size == 1, "function capture requires one circuit")
    val modules = circuits.head.modules.map(m => m.name -> m).toMap
    functions.foreach { a =>
      val module = modules(a.module).asInstanceOf[firrtl.ir.Module]
      val original = firrtl.ir.Circuit(firrtl.ir.NoInfo, Seq(module), module.name).serialize
      val lowered = StorageFunctionLowering(module)
      val systemVerilog = StorageFunctionCompiler(module, annotations)
      a.session.recordFunction(StorageFunctionDef(a.id, a.module, a.source,
        a.inputs, a.output, a.inputClasses, a.outputClass, original, lowered, systemVerilog))
    }
    annotations.flatMap {
      case _: StorageFunctionCaptureAnnotation => Seq.empty
      case pending: StoragePendingIRAnnotation => Seq(StorageIRAnnotation(StorageIRCodec.encode(pending.finish())))
      case other => Seq(other)
    }
  }
}
