package scala.meta.internal.metals

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util

import scala.jdk.CollectionConverters._
import scala.util.Try

import com.google.gson.JsonArray
import com.google.gson.JsonObject

case class JavaReceiverRange(startUtf16: Int, endUtf16: Int)
case class JavaReceiverTypesParams(
    uri: String,
    buildTargetId: String,
    sourceSha256: String,
    targets: util.List[JavaReceiverRange],
)

/** Bounded source/target identity for the Java-only semantic receiver route. */
object JavaReceiverTypes {
  val SchemaVersion = "metals-java-receiver-types.v2"
  val MaxSourceBytes: Int = 1024 * 1024
  val OptionsProfile = "metals-java-receiver-bsp-options.v1"

  /**
   * Remove only Bloop's SemanticDB instrumentation, not project semantics.
   * JVM exports are already supplied by the Metals launcher. Unknown forked
   * JVM flags cannot be reproduced in an in-process compiler and fail closed.
   */
  def compilerOptions(options: List[String]): Either[String, List[String]] = {
    val exports = Set("api", "code", "model", "tree", "util").map(name =>
      s"-Jjdk.compiler/com.sun.tools.javac.$name=ALL-UNNAMED"
    )
    def loop(
        rest: List[String],
        kept: List[String],
    ): Either[String, List[String]] =
      rest match {
        case "-J--add-exports" :: value :: tail if exports(value) =>
          loop(tail, kept)
        case head :: tail
            if head == "-Xplugin:semanticdb" ||
              head.startsWith("-Xplugin:semanticdb ") =>
          loop(tail, kept)
        case head :: _ if head.startsWith("-J") =>
          Left("UNSUPPORTED_FORKED_JAVA_OPTIONS")
        case head :: tail => loop(tail, head :: kept)
        case Nil => Right(kept.reverse)
      }
    loop(options, Nil)
  }

  def unavailable(reason: String): JsonObject = {
    val result = new JsonObject()
    result.addProperty("schemaVersion", SchemaVersion)
    result.addProperty("status", reason)
    result.add("documents", new JsonArray())
    result
  }

  def valid(params: JavaReceiverTypesParams): Boolean =
    params != null && params.uri != null && params.uri.length <= 8192 &&
      params.buildTargetId != null && params.buildTargetId.nonEmpty &&
      params.buildTargetId.length <= 8192 && params.sourceSha256 != null &&
      params.sourceSha256.matches("sha256:[0-9a-f]{64}") &&
      params.targets != null && !params.targets.isEmpty && params.targets.size <= 256 &&
      params.targets.asScala.forall(r =>
        r != null && r.startUtf16 >= 0 && r.endUtf16 > r.startUtf16 && r.endUtf16 <= MaxSourceBytes
      ) &&
      Try(URI.create(params.uri)).toOption.exists(uri =>
        uri.getScheme == "file" && uri.getRawQuery == null &&
          uri.getRawFragment == null && Option(uri.getRawAuthority).forall(
            _.isEmpty
          ) &&
          Option(uri.getPath).exists(_.endsWith(".java"))
      )

  def optionsHash(options: List[String]): String = {
    val values = new JsonArray()
    options.foreach(values.add)
    "sha256:" + MessageDigest
      .getInstance("SHA-256")
      .digest(values.toString.getBytes(UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
  }
}
