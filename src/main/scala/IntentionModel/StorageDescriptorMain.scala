// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/** Validate generated artifacts or regenerate C++ from saved JSON descriptors. */
object StorageDescriptorMain {
  def main(args: Array[String]): Unit = {
    require(args.length == 2 && Set("--check", "--emit")(args(0)),
      "Usage: StorageDescriptorMain (--check|--emit) <generated-src-directory>")
    val directory = Paths.get(args(1))
    val stream = Files.newDirectoryStream(directory, "storage-*.json")
    val files = try stream.iterator().asScala.toVector.sortBy(_.getFileName.toString) finally stream.close()
    val decoded = files.map(file => file -> StorageIRCodec.decode(new String(Files.readAllBytes(file), UTF_8)))
    decoded.foreach { case (file, model) =>
      require(file.getFileName.toString == s"storage-${model.id.value}.json", "descriptor filename/ID mismatch")
    }
    val models = decoded.map(_._2).sortBy(_.id.value)
    val expected = StorageDPIEmitter.cpp(models)
    val source = directory.resolve("storage_dpi.cpp")
    if (args(0) == "--check") {
      require(new String(Files.readAllBytes(source), UTF_8) == expected, "generated Storage DPI C++ is stale")
      println(s"Validated ${models.size} storage descriptors and generated DPI C++")
    } else {
      Files.write(source, expected.getBytes(UTF_8))
      println(s"Generated DPI C++ from ${models.size} storage descriptors")
    }
  }
}
