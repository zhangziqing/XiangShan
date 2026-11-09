// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import firrtl.annotations.{ModuleTarget, SingleTargetAnnotation}

/** Identifies inherited queue state without adding RTL ports or a wrapper.
  * Extraction closes its next-state and observation dependencies in the owning
  * module; registers outside these roots are owner state, not FIFO state.
  */
case class QueueContractAnnotation(
  target: ModuleTarget, entries: Int, numEnq: Int, numDeq: Int, stateRoots: Seq[String]
) extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(n: ModuleTarget): QueueContractAnnotation = copy(target = n)
}
