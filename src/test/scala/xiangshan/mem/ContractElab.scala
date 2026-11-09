// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.mem

import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan._

object ContractElab extends App {
  val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial({
    case XSCoreParamsKey => base(XSTileKey).head.copy(VirtualLoadQueueSize = args(1).toInt)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.CRITICAL, 0)
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug, here(DebugOptionsKey).FPGAPlatform,
      here(DebugOptionsKey).EnableXMR)
  })
  (new circt.stage.ChiselStage).execute(
    Array("--target", "chirrtl", "--target-dir", args(0)),
    Seq(ChiselGeneratorAnnotation(() =>
      if (args.lift(2).contains("async")) new VLoadQueueResetHarness
      else new VLoadQueueMiterHarness)))
}
