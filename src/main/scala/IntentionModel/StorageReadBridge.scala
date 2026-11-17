// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.util.HasBlackBoxInline

sealed trait StorageReadBridgeMode
object StorageReadBridgeMode {
  case object PerRead extends StorageReadBridgeMode
  case object PerStorage extends StorageReadBridgeMode
}

/** One pure current-state query per source read, independent of entry count. */
class StorageReadBridge(width: Int, storageId: StorageId, opId: OpId)
  extends BlackBox with HasBlackBoxInline {
  require(width > 0 && width <= 64, "DPI read width must be in 1..64")
  override def desiredName: String = s"StorageReadBridge_${storageId.value}_${opId.value}"
  val io = IO(new Bundle {
    val epoch = Input(Bool())
    val index = Input(UInt(64.W))
    val value = Output(UInt(width.W))
  })
  setInline(s"$desiredName.sv",
    s"""module $desiredName(
       |  input epoch, input [63:0] index,
       |  output logic [${width - 1}:0] value
       |);
       |  import "DPI-C" function longint unsigned ${StorageDPIEmitter.readName(storageId)}(
       |    input longint unsigned index, input bit epoch);
       |  always_comb begin
       |    value = $width'(${StorageDPIEmitter.readName(storageId)}(index, epoch));
       |  end
       |endmodule
       |""".stripMargin)
}
