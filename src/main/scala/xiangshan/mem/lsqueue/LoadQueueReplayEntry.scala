// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import chisel3._
import chisel3.util.log2Up
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.backend.Bundles.DynInst
import xiangshan.cache.HasDCacheParameters
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache.wpu.ReplayCarry

/** Unreset replay contents, written together on new or returned enqueue.
  * Entry indices stay aligned with the metadata and address stores.
  */
class LoadReplayPayload(implicit p: Parameters) extends XSBundle {
  val uop = new DynInst
  val vecReplay = new VecReplayInfo
}

/** Resettable entry control; every field starts at zero. */
class LoadReplayMeta(implicit p: Parameters) extends XSBundle
  with HasDCacheParameters with HasTlbConst {
  val allocated = Bool()
  val scheduled = Bool()
  val blocking = Bool()
  val strict = Bool()
  val cause = UInt(LoadReplayCauses.allCauses.W)
  val missMSHRId = UInt(log2Up(cfg.nMissEntries + 1).W)
  val tlbHintId = UInt(log2Up(loadfiltersize + 1).W)
  val replacementUpdated = Bool()
  val missDbUpdated = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val dataInLastBeat = Bool()
}
