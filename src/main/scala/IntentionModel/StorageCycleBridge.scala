// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.util.HasBlackBoxInline
import scala.collection.immutable.ListMap

class StorageCycleIO(widths: Vector[(Int, Int)]) extends Record {
  val elements = ListMap((Seq(
    "clock" -> Input(Clock()), "reset" -> Input(Bool()),
    "epoch" -> Output(Bool())
  ) ++ widths.zipWithIndex.flatMap { case ((index, value), i) => Seq(
    s"guard_$i" -> Input(Bool()), s"index_$i" -> Input(UInt(index.W)), s"value_$i" -> Input(UInt(value.W))
  ) }): _*)
  def clock: Clock = elements("clock").asInstanceOf[Clock]
  def reset: Bool = elements("reset").asInstanceOf[Bool]
  def epoch: Bool = elements("epoch").asInstanceOf[Bool]
  def guard(i: Int): Bool = elements(s"guard_$i").asInstanceOf[Bool]
  def index(i: Int): UInt = elements(s"index_$i").asInstanceOf[UInt]
  def value(i: Int): UInt = elements(s"value_$i").asInstanceOf[UInt]
}

/** Computes an unpublished state version at the edge; NBA switches read visibility.
  * Reads using the old epoch remain stable until downstream registers sample.
  */
class StorageCycleBridge(storageId: StorageId, widths: Vector[(Int, Int)], initialized: Boolean)
  extends BlackBox with HasBlackBoxInline {
  private val packing = StorageCyclePacking(widths)
  override def desiredName: String = s"StorageCycleBridge_${storageId.value}"
  val io = IO(new StorageCycleIO(widths))
  private val args = (0 until packing.words).map(i => s"input longint unsigned inputs_$i").mkString(", ")
  private val active = {
    val events = (if (initialized) Seq("reset") else Seq.empty) ++ widths.indices.map(i => s"guard_$i")
    if (events.isEmpty) "1'b0" else events.mkString(" || ")
  }
  private val packed = {
    var previous = 0
    packing.fields.flatMap { field =>
      val padding = if (field.offset > previous) Seq(s"${field.offset - previous}'d0") else Seq.empty
      previous = field.end
      padding :+ field.name
    }.reverse.mkString("{", ", ", "}")
  }
  private val inputs = (0 until packing.words).map { i =>
    val high = math.min((i + 1) * 64, packing.totalWidth) - 1
    s"64'(cycle_inputs[$high:${i * 64}])"
  }.mkString(", ")
  private val svPorts = widths.zipWithIndex.map { case ((index, value), i) =>
    s", input guard_$i, input [${index - 1}:0] index_$i, input [${value - 1}:0] value_$i"
  }.mkString
  setInline(s"$desiredName.sv",
    s"""module $desiredName(
       |  input clock, input reset,
       |  output bit epoch$svPorts
       |);
       |  import "DPI-C" function void ${StorageDPIEmitter.cycleName(storageId)}(input bit epoch, $args);
       |  wire [${packing.totalWidth - 1}:0] cycle_inputs = $packed;
       |  initial begin
       |    epoch = 0;
       |  end
       |  always @(posedge clock) begin
       |    if ($active) begin
       |      ${StorageDPIEmitter.cycleName(storageId)}(epoch, $inputs);
       |      epoch <= ~epoch;
       |    end
       |  end
       |endmodule
       |""".stripMargin)
}
