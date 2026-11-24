// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import circt.stage.{CIRCTTarget, CIRCTTargetAnnotation, FirtoolBinaryPath, FirtoolOption}
import firrtl.{AnnotationSeq, EmittedVerilogCircuitAnnotation, ir => F}
import firrtl.stage.{FirrtlCircuitAnnotation, OutputFileAnnotation}

/** Check the callback contract before optimization, then use Chisel's CIRCT phase. */
private[IntentionModel] object StorageFunctionCompiler {
  /** Replay descriptors captured before SystemVerilog was saved in the function. */
  def fromChirrtl(source: String): String = {
    val binary = firtoolresolver.Resolve(chisel3.BuildInfo.firtoolVersion.get, true)
      .fold(message => throw new IllegalStateException(message), _.path.toString)
    os.proc(binary, "-format=fir", "--disable-all-randomization").call(stdin = source).out.text()
  }

  private def unsupported(value: Any): Nothing =
    throw new IllegalArgumentException(s"unsupported pure function hardware: $value")

  private def dataType(t: F.Type): Unit = t match {
    case _: F.UIntType | _: F.SIntType | F.UnknownType => ()
    case F.BundleType(fields) => fields.foreach { f =>
      require(f.flip == F.Default, "function aggregates must be passive")
      dataType(f.tpe)
    }
    case F.VectorType(child, _) => dataType(child)
    case other => unsupported(other)
  }
  private def expression(e: F.Expression): Unit = e match {
    case _: F.Reference | _: F.UIntLiteral | _: F.SIntLiteral => ()
    case F.SubField(e, _, _) => expression(e)
    case F.SubIndex(e, _, _) => expression(e)
    case F.SubAccess(e, i, _) => expression(e); expression(i)
    case F.DoPrim(_, args, _, _) => args.foreach(expression)
    case F.Mux(c, t, f, _) => expression(c); expression(t); expression(f)
    case other => unsupported(other.serialize)
  }
  private def statement(s: F.Statement): Unit = s match {
    case F.Block(ss) => ss.foreach(statement)
    case F.DefWire(_, _, t) => dataType(t)
    case F.DefNode(_, _, e) => expression(e)
    case F.Connect(_, lhs, rhs) => expression(lhs); expression(rhs)
    case F.Conditionally(_, c, t, f) => expression(c); statement(t); statement(f)
    case F.EmptyStmt => ()
    case other => unsupported(other.serialize)
  }

  def apply(module: F.Module, annotations: AnnotationSeq): String = {
    module.ports.foreach(p => dataType(p.tpe))
    statement(module.body)
    val circuit = F.Circuit(F.NoInfo, Seq(module), module.name)
    val options = Seq(FirrtlCircuitAnnotation(circuit), OutputFileAnnotation(module.name),
      CIRCTTargetAnnotation(CIRCTTarget.SystemVerilog), FirtoolOption("--disable-all-randomization")) ++
      annotations.collect { case path: FirtoolBinaryPath => path }
    new circt.stage.phases.CIRCT().transform(options).collectFirst {
      case EmittedVerilogCircuitAnnotation(result) => result.value
    }.getOrElse(throw new IllegalStateException("CIRCT did not emit the callback module"))
  }
}
