// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.FirtoolOption
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

object StorageModelStage {
  /** Emit RTL and a self-contained generated-src directory for the existing
    * Difftest build, which discovers generated C++ files and adds their include path.
    */
  def emit(
    generator: StorageIRSession => RawModule,
    buildDirectory: Path,
    runtimeHeader: Path = Paths.get("src/test/scala/csrc/storage/storage.hpp")
  ): Vector[StorageIR] = {
    require(Files.isRegularFile(runtimeHeader), s"Storage runtime header is missing: $runtimeHeader")
    val build = buildDirectory.toAbsolutePath.normalize()
    val session = new StorageIRSession
    (new top.XiangShanStage).execute(
      Array("--target", "systemverilog", "--target-dir", build.resolve(".storage-stage").toString),
      Seq(ChiselGeneratorAnnotation(() => generator(session)), FirtoolOption("--split-verilog"),
        FirtoolOption(s"-o=${build.resolve("rtl")}")))
    val models = session.models
    val generated = build.resolve("generated-src")
    StorageDPIEmitter.emit(models, generated)
    Files.copy(runtimeHeader, generated.resolve("storage.hpp"), StandardCopyOption.REPLACE_EXISTING)
    models
  }
}
