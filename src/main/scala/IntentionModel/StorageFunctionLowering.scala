// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import firrtl.{ir => F}
import firrtl2.{ir => L}

/** Structural adaptation between Chisel 6.7 FIRRTL and firrtl2 6.0, restricted to
  * pure combinational hardware. Reject the entire region, including dead effects.
  */
private[IntentionModel] object StorageFunctionLowering {
  private def unsupported(value: Any): Nothing =
    throw new IllegalArgumentException(s"unsupported pure function hardware: $value")
  private def tpe(t: F.Type): L.Type = t match {
    case F.UIntType(F.IntWidth(w)) => L.UIntType(L.IntWidth(w))
    case F.SIntType(F.IntWidth(w)) => L.SIntType(L.IntWidth(w))
    case F.UIntType(F.UnknownWidth) => L.UIntType(L.UnknownWidth)
    case F.SIntType(F.UnknownWidth) => L.SIntType(L.UnknownWidth)
    case F.BundleType(fields) => L.BundleType(fields.map { f =>
      require(f.flip == F.Default, "function aggregates must be passive")
      L.Field(f.name, L.Default, tpe(f.tpe))
    })
    case F.VectorType(child, size) => L.VectorType(tpe(child), size)
    case F.UnknownType => L.UnknownType
    case other => unsupported(other)
  }
  private def expr(e: F.Expression): L.Expression = e match {
    case F.Reference(n, t) => L.Reference(n, tpe(t))
    case F.SubField(e, n, t) => L.SubField(expr(e), n, tpe(t))
    case F.SubIndex(e, i, t) => L.SubIndex(expr(e), i, tpe(t))
    case F.SubAccess(e, i, t) => L.SubAccess(expr(e), expr(i), tpe(t))
    case F.UIntLiteral(v, F.IntWidth(w)) => L.UIntLiteral(v, L.IntWidth(w))
    case F.SIntLiteral(v, F.IntWidth(w)) => L.SIntLiteral(v, L.IntWidth(w))
    case F.DoPrim(op, args, constants, t) =>
      L.DoPrim(firrtl2.PrimOps.fromString(op.serialize), args.map(expr), constants, tpe(t))
    case F.Mux(c, t, f, tp) => L.Mux(expr(c), expr(t), expr(f), tpe(tp))
    case other => unsupported(other.serialize)
  }
  private def stmt(s: F.Statement): L.Statement = s match {
    case F.Block(statements) => L.Block(statements.map(stmt))
    case F.DefWire(_, n, t) => L.DefWire(L.NoInfo, n, tpe(t))
    case F.DefNode(_, n, e) => L.DefNode(L.NoInfo, n, expr(e))
    case F.Connect(_, lhs, rhs) => L.Connect(L.NoInfo, expr(lhs), expr(rhs))
    case F.Conditionally(_, c, t, f) => L.Conditionally(L.NoInfo, expr(c), stmt(t), stmt(f))
    case F.EmptyStmt => L.EmptyStmt
    case other => unsupported(other.serialize)
  }
  def apply(module: F.Module): String = {
    val ports = module.ports.map(p => L.Port(L.NoInfo, p.name,
      if (p.direction == F.Input) L.Input else L.Output, tpe(p.tpe)))
    val circuit = L.Circuit(L.NoInfo,
      Seq(L.Module(L.NoInfo, module.name, ports, stmt(module.body))), module.name)
    // The dependency manager runs resolution, width inference, aggregate lowering,
    // conditional expansion and initialization checks in their required order.
    new firrtl2.stage.transforms.Compiler(firrtl2.stage.Forms.LowForm)
      .transform(firrtl2.CircuitState(circuit)).circuit.serialize
  }
}
