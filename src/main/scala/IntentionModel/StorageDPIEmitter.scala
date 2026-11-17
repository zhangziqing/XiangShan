// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** C++ consumes only sealed descriptors; SV/C signatures share this emitter. */
object StorageDPIEmitter {
  def readName(id: StorageId): String = s"intention_storage_read_${id.value}"
  def readBatchName(id: StorageId): String = s"intention_storage_read_batch_${id.value}"
  def cycleName(id: StorageId): String = s"intention_storage_cycle_${id.value}"
  def writeSpecs(ir: StorageIR): Vector[(Int, Int, Int)] = ir.operations.collect {
    case WriteOp(id, _, _, _, _, _) => (id.value, 0, ir.entry.totalWidth)
    case WriteFieldOp(id, _, _, _, path, _, _) =>
      val (_, offset, width) = StorageLayout.field(ir.entry, path)
      (id.value, offset, width)
  }

  def cpp(models: Vector[StorageIR]): String = {
    models.foreach(StorageIRValidation.requireDPI)
    require(models.map(_.id).distinct.size == models.size, "duplicate storage ID in executable")
    val ordered = models.sortBy(_.id.value)
    def u(bits: BigInt): String = s"UINT64_C(0x${bits.toString(16)})"
    val storageModels = ordered.map { ir =>
      val initial = ir.initial match {
        case ConstantInitialState(bits) =>
          s"static const Model_${ir.id.value}::State initial_${ir.id.value}{{${bits.map(u).mkString(", ")}}};"
        case UnspecifiedInitialState => ""
      }
      s"""using Model_${ir.id.value} = intention::StorageModel<${ir.entries}, ${ir.entry.totalWidth}>;
         |static std::array<Model_${ir.id.value}, 2> storage_${ir.id.value};
         |$initial""".stripMargin
    }.mkString("\n")
    val readFunctions = ordered.filter(_.operations.exists(_.isInstanceOf[ReadOp])).map { ir =>
      s"""extern "C" unsigned long long ${readName(ir.id)}(unsigned long long index, svBit epoch) {
         |  return storage_${ir.id.value}[epoch != 0].read(index);
         |}""".stripMargin
    }.mkString("\n")
    val readBatches = ordered.flatMap { ir =>
      val reads = ir.operations.collect { case r: ReadOp => r }
      if (reads.isEmpty) None else {
        val args = reads.indices.map(i =>
          s", unsigned long long index_$i, unsigned long long *value_$i").mkString
        val outputs = reads.indices.map(i => s"*value_$i = storage_${ir.id.value}[epoch != 0].read(index_$i);").mkString("\n")
        Some(s"""extern "C" void ${readBatchName(ir.id)}(svBit epoch$args) {
                |  $outputs
                |}""".stripMargin)
      }
    }.mkString("\n")
    val cycleFunctions = ordered.map { ir =>
      val specs = writeSpecs(ir)
      val packing = StorageCyclePacking.fromIR(ir)
      val args = (0 until packing.words).map(i => s"unsigned long long inputs_$i").mkString(", ")
      def operand(name: String): String = {
        val field = packing.field(name)
        val word = s"inputs_${field.word}"
        if (field.width == 64) word
        else {
          val shifted = if (field.shift == 0) word else s"($word >> ${field.shift})"
          s"($shifted & ${u(StorageLayout.mask(field.width))})"
        }
      }
      val reset = ir.initial match {
        case _: ConstantInitialState =>
          s"if (${operand("reset")}) { next.reset(initial_${ir.id.value}); return; }"
        case UnspecifiedInitialState => "(void)inputs_0;"
      }
      val writes = specs.zipWithIndex.map { case ((_, offset, width), i) =>
        s"next.write<$offset, $width>(${operand(s"guard_$i")} != 0, ${operand(s"index_$i")}, ${operand(s"value_$i")});"
      }.mkString("\n")
      s"""extern "C" void ${cycleName(ir.id)}(svBit epoch, $args) {
         |  auto &next = storage_${ir.id.value}[epoch == 0];
         |  $reset
         |  next = storage_${ir.id.value}[epoch != 0];
         |  $writes
         |}""".stripMargin
    }.mkString("\n")
    s"""// Generated from storage IR schema 1. Do not edit.
       |#include "storage.hpp"
       |#include "svdpi.h"
       |$storageModels
       |$readFunctions
       |$readBatches
       |$cycleFunctions
       |""".stripMargin
  }

  def emit(models: Vector[StorageIR], directory: Path): Unit = {
    val source = cpp(models)
    Files.createDirectories(directory)
    val stale = Files.newDirectoryStream(directory, "storage-*.json")
    try {
      val keep = models.map(ir => s"storage-${ir.id.value}.json").toSet
      val iterator = stale.iterator()
      while (iterator.hasNext) {
        val file = iterator.next()
        if (!keep(file.getFileName.toString)) {
          val old = StorageIRCodec.decode(new String(Files.readAllBytes(file), UTF_8))
          require(file.getFileName.toString == s"storage-${old.id.value}.json", "unexpected descriptor filename")
          Files.delete(file)
        }
      }
    } finally stale.close()
    models.foreach(ir => Files.write(directory.resolve(s"storage-${ir.id.value}.json"),
      StorageIRCodec.encode(ir).getBytes(UTF_8)))
    Files.write(directory.resolve("storage_dpi.cpp"), source.getBytes(UTF_8))
  }
}
