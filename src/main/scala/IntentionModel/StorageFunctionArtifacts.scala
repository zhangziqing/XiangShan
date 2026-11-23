// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** A replayable record of invocation, original circuit, lowering and C++ output. */
object StorageFunctionArtifacts {
  private[IntentionModel] lazy val bitsHeader: String = {
    val stream = Option(getClass.getResourceAsStream("/IntentionModel/bits.hpp"))
      .getOrElse(throw new IllegalStateException("missing packaged Bits runtime"))
    val source = scala.io.Source.fromInputStream(stream, "UTF-8")
    try source.mkString finally source.close()
  }

  def emit(functions: Vector[StorageFunctionDef], directory: Path): Unit = {
    import StorageIRCodec._
    val header = StorageFunctionCpp.header(functions)
    val runtime = if (functions.nonEmpty) Some(bitsHeader) else None
    Files.createDirectories(directory)
    val keep = functions.map(_.id.value.toString).toSet
    val generated = """function-([0-9]+)\.(json|chirrtl\.fir|lowered\.fir)""".r
    val stream = Files.newDirectoryStream(directory, "function-*")
    try {
      val iterator = stream.iterator()
      while (iterator.hasNext) {
        val path = iterator.next()
        path.getFileName.toString match {
          case generated(id, _) if !keep(id) => Files.delete(path)
          case _ => ()
        }
      }
    } finally stream.close()
    def write(name: String, value: String): Unit =
      Files.write(directory.resolve(name), value.getBytes(UTF_8))
    functions.sortBy(_.id.value).foreach { f =>
      write(s"function-${f.id.value}.json", upickle.default.write(f, indent = 2))
      write(s"function-${f.id.value}.chirrtl.fir", f.chirrtl)
      write(s"function-${f.id.value}.lowered.fir", f.firrtl)
    }
    if (functions.nonEmpty) {
      write("bits.hpp", runtime.get)
      write("storage_functions.hpp", header)
    } else {
      Files.deleteIfExists(directory.resolve("storage_functions.hpp"))
      Files.deleteIfExists(directory.resolve("bits.hpp"))
    }
  }
}
