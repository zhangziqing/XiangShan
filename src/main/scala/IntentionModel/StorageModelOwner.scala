// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import chisel3._
import scala.collection.mutable.ArrayBuffer

/** Opt-in lifecycle for directly instantiated software storage in this module.
  * Registration may occur in a superclass constructor, before this trait's body.
  */
trait StorageModelOwner extends Module with RequireSyncReset {
  private lazy val storageModels = ArrayBuffer.empty[() => Unit]
  private[IntentionModel] final def registerStorage(finish: () => Unit): Unit =
    storageModels += finish

  atModuleBodyEnd { storageModels.foreach(_()) }
}
