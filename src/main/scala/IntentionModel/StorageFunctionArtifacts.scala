// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.sys.process.{Process, ProcessLogger}

/** Replay captured circuits through the standard CIRCT and Verilator compilers. */
object StorageFunctionArtifacts {
  private def remove(path: Path): Unit = if (Files.exists(path)) {
    val files = Files.walk(path)
    try files.iterator().asScala.toVector.sortBy(_.getNameCount).reverse.foreach(Files.delete(_))
    finally files.close()
  }

  def emit(functions: Vector[StorageFunctionDef], directory: Path, models: Vector[StorageIR] = Vector.empty): Unit = {
    import StorageIRCodec._
    val batches = StorageFunctionCpp.batches(functions, models)
    val header = StorageFunctionCpp.header(batches)
    Files.createDirectories(directory)
    val keep = functions.map(_.id.value.toString).toSet
    val generated = """function-([0-9]+)\.(json|chirrtl\.fir|lowered\.fir)""".r
    val stream = Files.newDirectoryStream(directory, "function-*")
    try stream.iterator().asScala.foreach { path => path.getFileName.toString match {
      case generated(id, suffix) if !keep(id) || suffix == "lowered.fir" => Files.delete(path)
      case _ => ()
    }} finally stream.close()
    def write(name: String, value: String): Unit =
      Files.write(directory.resolve(name), value.getBytes(UTF_8))
    val ordered = functions.sortBy(_.id.value)
    ordered.foreach { f =>
      write(s"function-${f.id.value}.json", upickle.default.write(f, indent = 2))
      write(s"function-${f.id.value}.chirrtl.fir", f.chirrtl)
    }
    val output = directory.resolve("storage_functions")
    remove(output)
    Files.deleteIfExists(directory.resolve("bits.hpp"))
    if (functions.nonEmpty) {
      def port(f: StorageFunctionDef, name: String): String = StorageFunctionCpp.port(f.id, name)
      val ports = batches.flatMap { batch =>
        batch.widths.zipWithIndex.map { case (width, i) =>
          val lanes = if (i == 0) s" [${batch.entries}]" else ""
          s"input [${width - 1}:0] ${batch.input(i)}$lanes"
        } ++ batch.functions.map(f =>
          s"output [${f.output.totalWidth - 1}:0] ${port(f, "result")} [${batch.entries}]")
      }.mkString(",\n  ")
      val instances = batches.flatMap { batch => batch.functions.map { f =>
        val connections = f.inputs.zipWithIndex.map { case (layout, i) =>
          val lane = if (i == 0) "[lane]" else ""
          s".argument_$i(${batch.input(i)}$lane[${layout.totalWidth - 1}:0])"
        } :+ s".result(${port(f, "result")}[lane])"
        s"""  for (genvar lane = 0; lane < ${batch.entries}; ++lane) begin: function_${f.id.value}
           |    ${f.module} callback(${connections.mkString(", ")});
           |  end""".stripMargin
      }}.mkString("\n")
      val bodies = ordered.map(f => if (f.systemVerilog.nonEmpty) f.systemVerilog
        else StorageFunctionCompiler.fromChirrtl(f.chirrtl)).mkString("\n")
      write("storage_functions.sv", s"module IntentionFunctions(\n  $ports\n);\n$instances\nendmodule\n$bodies")
      val log = new java.lang.StringBuffer
      val status = Process(Seq("verilator", "--cc", "--no-timing", "--threads", "1",
        "--prefix", StorageFunctionCpp.prefix, "--top-module", "IntentionFunctions",
        "--Mdir", output.toAbsolutePath.toString, directory.resolve("storage_functions.sv").toAbsolutePath.toString))
        .!(ProcessLogger(line => log.append(line).append('\n'), line => log.append(line).append('\n')))
      require(status == 0, s"Verilator failed for Storage callbacks:\n$log")
      val files = Files.newDirectoryStream(output, "*.cpp")
      val sources = try files.iterator().asScala.toVector.sortBy(_.getFileName.toString).map { file =>
        s"#include \"${directory.relativize(file).toString}\""
      } finally files.close()
      write("storage_functions.hpp", header)
      write("storage_functions.inc", sources.mkString("\n") + "\n")
    } else {
      Seq("storage_functions.hpp", "storage_functions.inc", "storage_functions.sv")
        .foreach(name => Files.deleteIfExists(directory.resolve(name)))
    }
  }
}
