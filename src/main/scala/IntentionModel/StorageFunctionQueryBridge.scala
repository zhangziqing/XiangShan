// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import chisel3.util.HasBlackBoxInline
import scala.collection.immutable.ListMap

/** Result words cross the boundary packed; individual values never straddle a word. */
private[IntentionModel] final case class StorageQueryPacking(widths: Vector[Int]) {
  private var next = 0
  val fields: Vector[StorageCycleField] = widths.zipWithIndex.map { case (width, i) =>
    require(width > 0 && width <= 64, "query result width must be in 1..64")
    if (next % 64 + width > 64) next += 64 - next % 64
    val field = StorageCycleField(s"value_$i", width, next)
    next = field.end
    field
  }
  val words: Int = (next + 63) / 64
}

private[IntentionModel] class StorageFunctionQueryIO(
  parameters: Vector[Int], results: Vector[Int], exposeOverflow: Boolean
) extends Record {
  private val packing = StorageQueryPacking(results)
  val elements = ListMap((Seq("epoch" -> Input(Bool()), "enabled" -> Input(Bool()), "active" -> Input(Bool())) ++
    parameters.zipWithIndex.map { case (w, i) => s"parameter_$i" -> Input(UInt(w.W)) } ++
    (0 until packing.words).map(i => s"word_$i" -> Output(UInt(64.W))) ++
    (if (exposeOverflow) Seq("overflow" -> Output(Bool())) else Seq.empty)): _*)
  def overflow: Bool = elements("overflow").asInstanceOf[Bool]
  def epoch: Bool = elements("epoch").asInstanceOf[Bool]
  def active: Bool = elements("active").asInstanceOf[Bool]
  def enabled: Bool = elements("enabled").asInstanceOf[Bool]
  def parameter(i: Int): UInt = elements(s"parameter_$i").asInstanceOf[UInt]
  def value(i: Int): UInt = {
    val field = packing.fields(i)
    elements(s"word_${field.word}").asInstanceOf[UInt](field.shift + field.width - 1, field.shift)
  }

}

/** A combinational query, refreshed by explicit parameters and state publication. */
private[IntentionModel] class StorageFunctionQueryBridge(
  storage: StorageId, op: OpId, function: FunctionId, parameters: Vector[Int], results: Vector[Int],
  exposeOverflow: Boolean = false
) extends BlackBox with HasBlackBoxInline {
  require(results.nonEmpty && (parameters ++ results).forall(w => w > 0 && w <= 64))
  override def desiredName: String = s"StorageFunctionQuery_${storage.value}_${op.value}_fn_${function.value}"
  val io = IO(new StorageFunctionQueryIO(parameters, results, exposeOverflow))
  private val packing = StorageQueryPacking(results)
  private val ports = parameters.zipWithIndex.map { case (w, i) => s", input [${w-1}:0] parameter_$i" }.mkString +
    (0 until packing.words).map(i => s", output logic [63:0] word_$i").mkString +
    (if (exposeOverflow) ", output bit overflow" else "")
  private val args = parameters.indices.map(i => s", input longint unsigned parameter_$i").mkString +
    (0 until packing.words).map(i => s", output longint unsigned bits_$i").mkString
  private val locals = (0 until packing.words).map(i => s"longint unsigned bits_$i;").mkString("\n")
  private val call = parameters.indices.map(i => s", parameter_$i").mkString +
    (0 until packing.words).map(i => s", bits_$i").mkString
  private val outputs = (0 until packing.words).map(i => s"word_$i = bits_$i;").mkString("\n")
  private val overflowDeclaration = if (exposeOverflow) "" else "bit overflow;"
  private val overflowCheck = if (exposeOverflow) "" else
    "assert (!enabled || !overflow) else $error(\"camRead hit count exceeds maxHits\");"
  setInline(s"$desiredName.sv", s"""module $desiredName(input epoch, input enabled, input active$ports);
    |  import "DPI-C" function void ${StorageFunctionQueries.name(storage, op)}(input bit epoch$args, output bit overflow);
    |  $locals
    |  $overflowDeclaration
    |  always_comb begin
    |    if (active) begin
    |      ${StorageFunctionQueries.name(storage, op)}(epoch$call, overflow);
    |      $outputs
    |      $overflowCheck
    |    end else begin
    |      ${(0 until packing.words).map(i => s"word_$i = 64'd0;").mkString("\n")}
    |      overflow = 0;
    |    end
    |  end
    |endmodule
    |""".stripMargin)
}
