// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.backend.Bundles.DynInst

/** Unreset replay contents, written together on new or returned enqueue.
  * Entry indices stay aligned with the metadata and address stores.
  */
class LoadReplayPayload(implicit p: Parameters) extends XSBundle {
  val uop = new DynInst
  val vecReplay = new VecReplayInfo
}
