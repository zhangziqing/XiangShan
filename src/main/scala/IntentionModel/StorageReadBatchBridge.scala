// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.util.HasBlackBoxInline
import scala.collection.immutable.ListMap

private[IntentionModel] class StorageReadBatchIO(width: Int, count: Int) extends Record {
  val elements = ListMap((Seq("epoch" -> Input(Bool())) ++ (0 until count).flatMap(i => Seq(
    s"index_$i" -> Input(UInt(64.W)), s"value_$i" -> Output(UInt(width.W))
  ))): _*)
  def epoch: Bool = elements("epoch").asInstanceOf[Bool]
  def index(i: Int): UInt = elements(s"index_$i").asInstanceOf[UInt]
  def value(i: Int): UInt = elements(s"value_$i").asInstanceOf[UInt]
}

/** All read sites share one query boundary. Any address change refreshes every port. */
private[IntentionModel] class StorageReadBatchBridge(width: Int, storageId: StorageId, count: Int)
  extends BlackBox with HasBlackBoxInline {
  require(width > 0 && width <= 64, "DPI read width must be in 1..64")
  require(count > 0, "A batch read bridge requires at least one read")
  override def desiredName: String = s"StorageReadBatchBridge_${storageId.value}"
  val io = IO(new StorageReadBatchIO(width, count))
  private val ports = (0 until count).map(i =>
    s", input [63:0] index_$i, output logic [${width - 1}:0] value_$i").mkString
  private val args = (0 until count).map(i =>
    s", input longint unsigned index_$i, output longint unsigned bits_$i").mkString
  private val words = (0 until count).map(i => s"longint unsigned bits_$i;").mkString("\n")
  private val inputs = (0 until count).map(i => s", index_$i, bits_$i").mkString
  private val outputs = (0 until count).map(i => s"value_$i = bits_$i[${width - 1}:0];").mkString("\n")
  setInline(s"$desiredName.sv",
    s"""module $desiredName(input epoch$ports);
       |  import "DPI-C" function void ${StorageDPIEmitter.readBatchName(storageId)}(input bit epoch$args);
       |  $words
       |  always_comb begin
       |    ${StorageDPIEmitter.readBatchName(storageId)}(epoch$inputs);
       |    $outputs
       |  end
       |endmodule
       |""".stripMargin)
}
