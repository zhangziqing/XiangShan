// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

/** Preserve the packed callback ABI around Verilator's public model interface. */
object StorageFunctionCpp {
  def name(id: FunctionId): String = s"intention_function_${id.value}"
  def batchName(id: FunctionId): String = s"${name(id)}_batch"
  private[IntentionModel] val prefix = "VIntentionFunctions"
  private[IntentionModel] def port(id: FunctionId, member: String): String = s"function_${id.value}_$member"
  private def mask(width: Int): String = {
    require(width > 0 && width <= 64, "function arguments and result must fit in 64 bits")
    s"UINT64_C(0x${((BigInt(1) << width) - 1).toString(16)})"
  }

  private[IntentionModel] final case class Batch(
    name: String, entries: Int, functions: Vector[StorageFunctionDef]
  ) {
    val widths: Vector[Int] = (0 until functions.map(_.inputs.size).max).map { i =>
      functions.filter(_.inputs.size > i).map(_.inputs(i).totalWidth).max
    }.toVector
    def input(i: Int): String = s"${name}_argument_$i"
  }

  private[IntentionModel] def batches(
    functions: Vector[StorageFunctionDef], models: Vector[StorageIR]
  ): Vector[Batch] = functions.map { f =>
    val users = models.filter(_.functions.exists(_.id == f.id))
    val group = users.headOption.map(ir => s"storage_${ir.id.value}").getOrElse(s"function_${f.id.value}")
    (group, users.map(_.entries).foldLeft(1)(math.max), f)
  }.groupBy(_._1).toVector.sortBy(_._1).map { case (group, members) =>
    Batch(group, members.map(_._2).max, members.map(_._3).sortBy(_.id.value))
  }

  def function(f: StorageFunctionDef): String = function(f, Batch(s"function_${f.id.value}", 1, Vector(f)))

  private def function(f: StorageFunctionDef, batch: Batch): String = {
    require(f.id.value >= 0 && f.inputs.nonEmpty, "invalid function identity or signature")
    f.inputs.foreach(layout => mask(layout.totalWidth))
    val parameters = f.inputs.indices.drop(1).map(i => s", std::uint64_t argument_$i").mkString
    val arguments = f.inputs.indices.drop(1).map(i => s", argument_$i").mkString
    val assignments = f.inputs.indices.drop(1).map { i =>
      s"""    const auto parameter_$i = argument_$i & ${mask(batch.widths(i))};
         |    changed |= model.${batch.input(i)} != parameter_$i;
         |    model.${batch.input(i)} = parameter_$i;""".stripMargin
    }.mkString("\n")
    s"""struct ${batchName(f.id)} {
       |  template<class Entry, std::size_t Entries>
       |  std::array<std::uint64_t, Entries> operator()(const std::array<Entry, Entries>& entries$parameters) const {
       |    static_assert(Entries <= ${batch.entries}, "callback batch exceeds compiled capacity");
       |    intention_function_detail::RestoreContext restore;
       |    auto& state = intention_function_detail::state();
       |    auto& model = state.model;
       |    bool changed = !state.initialized;
       |$assignments
       |    for (std::size_t i = 0; i < Entries; ++i) {
       |      const auto value = entries[i] & ${mask(batch.widths.head)};
       |      changed |= model.${batch.input(0)}[i] != value;
       |      model.${batch.input(0)}[i] = value;
       |    }
       |    if (changed) { model.eval(); state.initialized = true; }
       |    std::array<std::uint64_t, Entries> result{};
       |    for (std::size_t i = 0; i < Entries; ++i)
       |      result[i] = model.${port(f.id, "result")}[i] & ${mask(f.output.totalWidth)};
       |    return result;
       |  }
       |};
       |static inline std::uint64_t ${name(f.id)}(std::uint64_t argument_0$parameters) {
       |  return ${batchName(f.id)}{}(std::array<std::uint64_t, 1>{{argument_0}}$arguments)[0];
       |}""".stripMargin
  }

  private[IntentionModel] def header(batches: Vector[Batch]): String = {
    val functions = batches.flatMap(_.functions)
    require(functions.map(_.id).distinct.size == functions.size, "duplicate function ID")
    s"""// Generated pure combinational functions. Do not edit.
       |#pragma once
       |#include <array>
       |#include <cstdint>
       |#include "storage_functions/$prefix.h"
       |namespace intention_function_detail {
       |struct RestoreContext {
       |  VerilatedContext* previous = Verilated::threadContextp();
       |  ~RestoreContext() { Verilated::threadContextp(previous); }
       |};
       |struct State {
       |  VerilatedContext context;
       |  $prefix model{&context};
       |  bool initialized = false;
       |};
       |static inline State& state() {
       |  static thread_local State state;
       |  return state;
       |}
       |}
       |${batches.flatMap(b => b.functions.map(f => function(f, b))).mkString("\n")}
       |""".stripMargin
  }
}
