// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

/** Preserve the packed callback ABI around Verilator's public model interface. */
object StorageFunctionCpp {
  def name(id: FunctionId): String = s"intention_function_${id.value}"
  private[IntentionModel] val prefix = "VIntentionFunctions"
  private[IntentionModel] def port(id: FunctionId, member: String): String = s"function_${id.value}_$member"
  private def mask(width: Int): String = {
    require(width > 0 && width <= 64, "function arguments and result must fit in 64 bits")
    s"UINT64_C(0x${((BigInt(1) << width) - 1).toString(16)})"
  }

  def function(f: StorageFunctionDef): String = {
    require(f.id.value >= 0 && f.inputs.nonEmpty, "invalid function identity or signature")
    val parameters = f.inputs.indices.map(i => s"std::uint64_t argument_$i").mkString(", ")
    val assignments = f.inputs.zipWithIndex.map { case (layout, i) =>
      s"  model.${port(f.id, s"argument_$i")} = argument_$i & ${mask(layout.totalWidth)};"
    }.mkString("\n")
    s"""static inline std::uint64_t ${name(f.id)}($parameters) {
       |  struct RestoreContext {
       |    VerilatedContext* previous = Verilated::threadContextp();
       |    ~RestoreContext() { Verilated::threadContextp(previous); }
       |  } restore;
       |  auto& model = intention_function_detail::model();
       |$assignments
       |  model.eval();
       |  return model.${port(f.id, "result")} & ${mask(f.output.totalWidth)};
       |}""".stripMargin
  }

  def header(functions: Vector[StorageFunctionDef]): String = {
    require(functions.map(_.id).distinct.size == functions.size, "duplicate function ID")
    val ordered = functions.sortBy(_.id.value)
    s"""// Generated pure combinational functions. Do not edit.
       |#pragma once
       |#include <cstdint>
       |#include "storage_functions/$prefix.h"
       |namespace intention_function_detail {
       |static inline $prefix& model() {
       |  struct State {
       |    VerilatedContext context;
       |    $prefix model{&context};
       |  };
       |  static thread_local State state;
       |  return state.model;
       |}
       |}
       |${ordered.map(function).mkString("\n")}
       |""".stripMargin
  }
}
