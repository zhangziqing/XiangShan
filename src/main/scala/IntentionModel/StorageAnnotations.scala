// SPDX-License-Identifier: MulanPSL-2.0
package IntentionModel

import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.options.Unserializable

final case class StorageReadResultAnnotation(
  target: ReferenceTarget, storageId: Long, opId: Int, resultId: Int
) extends SingleTargetAnnotation[ReferenceTarget] with Unserializable {
  def duplicate(n: ReferenceTarget): StorageReadResultAnnotation = copy(target = n)
}

/** Stage consumes these exact capture anchors before calling firtool. */
private[IntentionModel] final case class StorageWiringAnnotation(
  module: String,
  wires: Vector[String],
  instances: Vector[String],
  defaults: Vector[ReferenceTarget],
  connections: Vector[(ReferenceTarget, ReferenceTarget)]
) extends NoTargetAnnotation with Unserializable

final case class StorageIRAnnotation(json: String) extends NoTargetAnnotation with Unserializable {
  def ir: StorageIR = StorageIRCodec.decode(json)
}
