// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import firrtl.annotations.{JsonProtocol, ModuleTarget, SingleTargetAnnotation}
import firrtl.options.CustomFileEmission

/** Identifies inherited queue state without adding RTL ports or a wrapper.
  * Extraction closes its next-state and observation dependencies in the owning
  * module; registers outside these roots are owner state, not FIFO state.
  * CHIRRTL emission writes metadata separately; CIRCT does not receive it.
  */
case class QueueContractAnnotation(
  target: ModuleTarget, entries: Int, numEnq: Int, numDeq: Int, stateRoots: Seq[String]
) extends SingleTargetAnnotation[ModuleTarget] with CustomFileEmission {
  def duplicate(n: ModuleTarget): QueueContractAnnotation = copy(target = n)
  protected def baseFileName(annotations: firrtl.AnnotationSeq): String =
    s"${target.circuit}.${target.module}.queue-contract"
  protected def suffix: Option[String] = Some(".json")
  def getBytes: Iterable[Byte] = JsonProtocol.serialize(Seq(this)).getBytes("UTF-8")
}
