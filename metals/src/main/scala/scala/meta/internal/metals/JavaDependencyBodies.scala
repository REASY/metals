package scala.meta.internal.metals

import java.util

import com.google.gson.JsonObject

case class JavaDependencyBodiesParams(
    uri: String,
    buildTargetId: String,
    sourceSha256: String,
    targets: util.List[JavaReceiverRange],
    maxInventoryEntries: Int,
    compilerClasspathSha256: String,
    phase: String = "ACQUIRE",
    compilerContentSha256: String = null,
) {
  def receiverParams: JavaReceiverTypesParams =
    JavaReceiverTypesParams(uri, buildTargetId, sourceSha256, targets)
}

/** Separate service contract; dependency body sets are always open. */
object JavaDependencyBodies {
  val SchemaVersion = "metals-java-dependency-bodies.v2"
  def valid(params: JavaDependencyBodiesParams): Boolean =
    params != null && JavaReceiverTypes.valid(params.receiverParams) &&
      params.maxInventoryEntries >= 1 && params.maxInventoryEntries <= 262144 &&
      (if (params.phase == "FREEZE")
         params.compilerClasspathSha256 == null &&
         params.compilerContentSha256 == null
       else
         params.phase == "ACQUIRE" &&
         Option(params.compilerClasspathSha256).exists(
           _.matches("sha256:[0-9a-f]{64}")
         ) &&
         Option(params.compilerContentSha256).exists(
           _.matches("sha256:[0-9a-f]{64}")
         ))

  def unavailable(reason: String): JsonObject = {
    val result = JavaReceiverTypes.unavailable(reason)
    result.addProperty("schemaVersion", SchemaVersion)
    result
  }
}
