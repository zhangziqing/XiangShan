// SPDX-License-Identifier: MulanPSL-2.0
// Per-entry StoreQueue metadata.
package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.DynInst

/** Metadata retained by one StoreQueue element.
  *
  * Dispatch allocation patches its own fields; later indexed writes and
  * content-based updates follow source order. No slot identity is stored.
  */
class StoreQueueEntry(implicit p: Parameters) extends XSBundle {
  val uop = new DynInst

  val allocated = Bool()
  val completed = Bool()
  val addrvalid = Bool()
  val datavalid = Bool()
  val committed = Bool()
  val unaligned = Bool()
  val cross16Byte = Bool()
  val pending = Bool()
  val nc = Bool()
  val mmio = Bool()
  val memBackTypeMM = Bool()
  val prefetch = Bool()
  val isVec = Bool()
  val vecLastFlow = Bool()
  val vecMbCommit = Bool()
  val hasException = Bool()
  val waitStoreS2 = Bool()

  val debug_paddr = UInt(PAddrBits.W)
  val debug_vaddr = UInt(VAddrBits.W)
  val debug_data = UInt(XLEN.W)
  val debug_vec_unaligned_start = UInt(log2Up(XLEN).W)
  val debug_vec_unaligned_offset = UInt(log2Up(XLEN).W)
}
