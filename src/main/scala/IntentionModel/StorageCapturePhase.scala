// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import firrtl.AnnotationSeq
import firrtl.annotations.{ReferenceTarget, TargetToken}
import firrtl.ir.{UIntType => FirrtlUIntType, _}
import firrtl.options.{Dependency, Phase}
import firrtl.stage.FirrtlCircuitAnnotation
import xiangshan.transforms.Helpers._

/** Lift only explicitly annotated capture declarations, preserving guarded connects.
  * This runs on elaborated FIRRTL, before CIRCT; it never recognizes storage patterns.
  */
class StorageCapturePhase extends Phase {
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
