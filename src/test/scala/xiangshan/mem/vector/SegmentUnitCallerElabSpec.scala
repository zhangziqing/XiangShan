/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.mem

import chisel3.emitVerilog
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan._

/** Caller-level elaboration proof for the SegmentRing integration: the real
  * `VSegmentUnit` parent must lower through the full Chisel-to-Verilog flow, so
  * an undriven ring port or a mis-wired pointer fails the build.
  */
class SegmentUnitCallerElabSpec extends AnyFlatSpec {
  private val base = new DefaultConfig
  private val config = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.CRITICAL,
      perfDBHartID = 0
    )
  }.alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform,
      here(DebugOptionsKey).EnableXMR
    )
  })

  behavior of "VSegmentUnit SegmentRing caller"

  it should "elaborate the full caller wiring" in {
    emitVerilog(new VSegmentUnit()(config), Array("--target-dir", "test_run_dir/owned-elab/VSegmentUnit"))
  }
}
