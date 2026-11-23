// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import firrtl2.ir._
import scala.collection.mutable

/** Compile validated, scalar low FIRRTL into typed C++ bit-vector operations.
  * Public arguments/results fit in 64 bits; internal temporaries fit in 128.
  * The Bits runtime owns truncation, extension, arithmetic and shift semantics.
  */
object StorageFunctionCpp {
  def name(id: FunctionId): String = s"intention_function_${id.value}"
  private val cppReserved = """
    alignas alignof and and_eq asm auto bitand bitor bool break case catch char
    char8_t char16_t char32_t class compl concept const consteval constexpr constinit
    const_cast continue co_await co_return co_yield decltype default delete do double
    dynamic_cast else enum explicit export extern false float for friend goto if
    inline int long mutable namespace new noexcept not not_eq nullptr operator or
    or_eq private protected public register reinterpret_cast requires return short
    signed sizeof static static_assert static_cast struct switch template this
    thread_local throw true try typedef typeid typename union unsigned using virtual
    void volatile wchar_t while xor xor_eq
    Bits mask sext shift_left shift_right shift_signed parity std intention_function_detail intention
  """.trim.split("\\s+").toSet
  private def cppIdentifier(n: String): Boolean =
    n.matches("[A-Za-z_][A-Za-z0-9_]*") && !n.contains("__") &&
      !n.matches("_[A-Z].*") && !cppReserved(n) &&
      !n.matches("U?INT.*_(MIN|MAX|C)|PTRDIFF_(MIN|MAX)|SIG_ATOMIC_(MIN|MAX)|SIZE_MAX|WCHAR_(MIN|MAX)|WINT_(MIN|MAX)")

  private def localNames(names: Iterable[String], inputs: Set[String]): Map[String, String] = {
    val ordered = names.toVector.distinct.sorted
    // Reserve every usable original name before allocating aliases, including
    // names whose definitions will be emitted later during dependency traversal.
    val used = mutable.Set.empty[String] ++ ordered.filter(cppIdentifier)
    def fresh(base: String): String = {
      var candidate = base
      var suffix = 0
      while (used(candidate) || !cppIdentifier(candidate)) {
        suffix += 1
        candidate = s"${base}_$suffix"
      }
      used += candidate
      candidate
    }
    ordered.map { n =>
      val local = if (inputs(n)) fresh(s"${n}_bits")
      else if (cppIdentifier(n)) n
      else {
        val stem = n.replaceAll("[^A-Za-z0-9_]", "_").replaceAll("_+", "_")
          .stripPrefix("_").stripSuffix("_")
        fresh(s"firrtl_${if (stem.isEmpty) "value" else stem}")
      }
      n -> local
    }.toMap
  }

  private def width(t: Type): Int = {
    val w = t match {
      case UIntType(IntWidth(n)) => require(n > 0 && n <= 128, "invalid function width"); n.toInt
      case SIntType(IntWidth(n)) => require(n > 0 && n <= 128, "invalid function width"); n.toInt
      case other => throw new IllegalArgumentException(s"non-scalar function type: $other")
    }
    require(w > 0 && w <= 128, "function temporary width must be in 1..128")
    w
  }
  private def signed(t: Type): Boolean = t.isInstanceOf[SIntType]
  private def cppType(t: Type): String = s"Bits<${width(t)}${if (signed(t)) ", true" else ""}>"
  private def constant(v: BigInt, t: Type): String = {
    val bits = v & ((BigInt(1) << width(t)) - 1)
    val lo = bits & ((BigInt(1) << 64) - 1)
    val hi = bits >> 64
    val low = s"UINT64_C(0x${lo.toString(16)})"
    if (hi == 0) s"${cppType(t)}{$low}"
    else s"${cppType(t)}::from_words(UINT64_C(0x${hi.toString(16)}), $low)"
  }
  def function(f: StorageFunctionDef): String = {
    require(f.id.value >= 0 && f.inputs.nonEmpty, "invalid function identity or signature")
    val circuit = firrtl2.passes.InferTypes.run(firrtl2.Parser.parse(f.firrtl))
    require(circuit.modules.size == 1 && circuit.main == f.module, "invalid function module")
    val module = circuit.modules.head.asInstanceOf[Module]
    val inputs = module.ports.filter(_.direction == Input)
    val outputs = module.ports.filter(_.direction == Output)
    require(inputs.map(_.name) == f.inputs.indices.map(i => s"argument_$i"), "function argument names mismatch")
    require(inputs.map(p => width(p.tpe)) == f.inputs.map(_.totalWidth), "function input layout mismatch")
    require(outputs.size == 1 && outputs.head.name == "result" && width(outputs.head.tpe) == f.output.totalWidth,
      "function result layout mismatch")
    require(f.inputs.forall(_.totalWidth <= 64) && f.output.totalWidth <= 64,
      "function arguments and result must fit in 64 bits")
    val types = mutable.Map.empty[String, Type]
    val drivers = mutable.Map.empty[String, Expression]
    module.ports.foreach(p => types(p.name) = p.tpe)
    def drive(n: String, t: Type, e: Expression): Unit = {
      require(!drivers.contains(n) && !inputs.exists(_.name == n), s"multiple or input driver: $n")
      width(t); types(n) = t; drivers(n) = e
    }
    def collect(s: Statement): Unit = s match {
      case Block(ss) => ss.foreach(collect)
      case DefWire(_, n, t) => width(t); types(n) = t
      case DefNode(_, n, e) => drive(n, e.tpe, e)
      case Connect(_, Reference(n, t, _, _), e) => drive(n, t, e)
      case EmptyStmt => ()
      case other => throw new IllegalArgumentException(s"unsupported lowered function statement: ${other.serialize}")
    }
    collect(module.body)
    val names = localNames(types.keys, inputs.map(_.name).toSet)
    val lines = mutable.ArrayBuffer.empty[String]
    val values = mutable.Map.empty[String, String]
    val visiting = mutable.Set.empty[String]
    def reference(n: String): String = values.getOrElseUpdate(n, {
      require(visiting.add(n), s"combinational cycle in function ${f.id.value}: $n")
      val t = types.getOrElse(n, throw new IllegalArgumentException(s"external function reference: $n"))
      val body = if (inputs.exists(_.name == n)) s"${cppType(t)}{$n}" else {
        expression(drivers.getOrElse(n, throw new IllegalArgumentException(s"undriven function value: $n")))
      }
      val local = names(n)
      val origin = if (local == n) "" else s" // FIRRTL: `$n`"
      lines += s"  const ${cppType(t)} $local = $body; (void)$local;$origin"
      visiting -= n
      local
    })
    def expression(e: Expression): String = {
      width(e.tpe)
      e match {
        case Reference(n, _, _, _) => reference(n)
        case UIntLiteral(v, _) => constant(v, e.tpe)
        case SIntLiteral(v, _) => constant(v, e.tpe)
        case Mux(c, t, f, _) => s"intention::mux(${expression(c)}, ${expression(t)}, ${expression(f)})"
        case DoPrim(op, args, constants, _) =>
          def a(i: Int): String = expression(args(i))
          def c(i: Int): BigInt = constants(i)
          def method(i: Int, call: String): String = s"(${a(i)}).$call"
          op.serialize match {
            case "add" => s"(${a(0)} + ${a(1)})"
            case "sub" => s"(${a(0)} - ${a(1)})"
            case "mul" => s"(${a(0)} * ${a(1)})"
            case "and" => s"(${a(0)} & ${a(1)})"
            case "or" => s"(${a(0)} | ${a(1)})"
            case "xor" => s"(${a(0)} ^ ${a(1)})"
            case "not" => s"~${a(0)}"
            case "neg" => s"-${a(0)}"
            case "eq" => s"(${a(0)} == ${a(1)})"
            case "neq" => s"(${a(0)} != ${a(1)})"
            case "lt" => s"(${a(0)} < ${a(1)})"
            case "leq" => s"(${a(0)} <= ${a(1)})"
            case "gt" => s"(${a(0)} > ${a(1)})"
            case "geq" => s"(${a(0)} >= ${a(1)})"
            case "asUInt" => method(0, "as_uint()")
            case "asSInt" => method(0, "as_sint()")
            case "cvt" => method(0, "cvt()")
            case "pad" => method(0, s"pad<${c(0)}>()")
            case "cat" => method(0, s"concat(${a(1)})")
            case "bits" => method(0, s"slice<${c(0)}, ${c(1)}>()")
            case "head" => method(0, s"head<${c(0)}>()")
            case "tail" => method(0, s"tail<${c(0)}>()")
            case "shl" => method(0, s"shl<${c(0)}>()")
            case "shr" => method(0, s"shr<${c(0)}>()")
            case "dshl" => s"(${a(0)} << ${a(1)})"
            case "dshr" => s"(${a(0)} >> ${a(1)})"
            case "andr" => method(0, "all()")
            case "orr" => method(0, "any()")
            case "xorr" => method(0, "parity()")
            case other => throw new IllegalArgumentException(s"unsupported function primitive: $other")
          }
        case other => throw new IllegalArgumentException(s"unsupported function expression: ${other.serialize}")
      }
    }
    // Validate all remaining definitions, not only the return cone.
    drivers.keys.toVector.sorted.foreach(reference)
    val result = reference(outputs.head.name)
    val parameters = inputs.map(p => s"std::uint64_t ${p.name}").mkString(", ")
    s"""static inline std::uint64_t ${name(f.id)}($parameters) {
       |  using intention::Bits;
       |${inputs.map(p => s"  (void)${p.name};").mkString("\n")}
       |${lines.mkString("\n")}
       |  return $result.to_uint64();
       |}""".stripMargin
  }
  def header(functions: Vector[StorageFunctionDef]): String = {
    require(functions.map(_.id).distinct.size == functions.size, "duplicate function ID")
    s"""// Generated pure combinational functions. Do not edit.
       |#pragma once
       |#include <cstdint>
       |#include "bits.hpp"
       |${functions.sortBy(_.id.value).map(function).mkString("\n")}
       |""".stripMargin
  }
}
