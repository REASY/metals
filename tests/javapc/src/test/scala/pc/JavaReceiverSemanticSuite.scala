package pc

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters._

import scala.meta.internal.jpc.JavaReceiverTarget
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.VirtualFileParams

import com.google.gson.JsonObject
import tests.pc.BaseJavaPCSuite

class JavaReceiverSemanticSuite extends BaseJavaPCSuite {
  override protected def javacOptions: List[String] =
    List("--release", "17", "-proc:none")
  private def request(source: String, expressions: List[String]): JsonObject = {
    val uri = URI.create("file:///Demo.java")
    val hash = "sha256:" + MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    val targets = expressions.map { expression =>
      val start = source.indexOf(expression)
      assert(start >= 0)
      JavaReceiverTarget(uri, hash, start, start + expression.length)
    }
    val inputs = List[VirtualFileParams](CompilerVirtualFileParams(uri, source))
    val result = presentationCompiler
      .javaReceiverTypes(inputs.asJava, targets.asJava)
      .get(30, TimeUnit.SECONDS)
    assertEquals(
      result.get("schemaVersion").getAsString,
      "metals-java-receiver-types.v2",
    )
    assertEquals(
      result,
      presentationCompiler
        .javaReceiverTypes(inputs.asJava, targets.reverse.asJava)
        .get(30, TimeUnit.SECONDS),
    )
    result
  }
  private def rows(result: JsonObject): List[JsonObject] = result
    .getAsJsonArray("documents")
    .get(0)
    .getAsJsonObject
    .getAsJsonArray("receiverSites")
    .asScala
    .map(_.getAsJsonObject)
    .toList

  test("same-arity-overloads-have-distinct-selected-descriptors") {
    val result = request(
      "class Demo { void run(java.util.List<String> xs) { xs.remove(0); xs.remove(\"x\"); } }",
      List("xs.remove(0)", "xs.remove(\"x\")"),
    )
    assertEquals(
      rows(result)
        .map(
          _.getAsJsonObject("selectedCallable").get("descriptor").getAsString
        )
        .toSet,
      Set("(I)Ljava/lang/Object;", "(Ljava/lang/Object;)Z"),
    )
    assert(
      rows(result).forall(
        _.getAsJsonObject("selectedCallable")
          .get("owner")
          .getAsString == "java.util.List"
      )
    )
  }
  test("constructors-and-implicit-calls-retain-selection-without-a-receiver") {
    val result = request(
      "class Demo { void f(int x) {} void f(String x) {} void run(Throwable t) { new RuntimeException(t); new RuntimeException(\"x\"); f(1); f(\"x\"); } }",
      List(
        "new RuntimeException(t)",
        "new RuntimeException(\"x\")",
        "f(1)",
        "f(\"x\")",
      ),
    )
    assertEquals(
      rows(result).map(_.get("status").getAsString).toSet,
      Set("SELECTED"),
    )
    assert(rows(result).forall(!_.has("receiverType")))
    assertEquals(
      rows(result)
        .map(
          _.getAsJsonObject("selectedCallable").get("descriptor").getAsString
        )
        .toSet,
      Set("(Ljava/lang/Throwable;)V", "(Ljava/lang/String;)V", "(I)V"),
    )
    assertEquals(
      rows(result).count(
        _.getAsJsonObject("selectedCallable")
          .get("member")
          .getAsString == "<init>"
      ),
      2,
    )
  }
  test("generic-chain-receiver-is-not-declaration-return-variable") {
    val result = request(
      "class Demo { <T> T id(T v) { return v; } void run(java.util.Map<String,String> map) { id(map).put(\"k\",\"v\"); } }",
      List("id(map).put(\"k\",\"v\")"),
    )
    val row = rows(result).head
    assertEquals(
      row.getAsJsonObject("receiverType").get("qualifiedName").getAsString,
      "java.util.Map",
    )
    assertEquals(
      row
        .getAsJsonObject("receiverType")
        .getAsJsonArray("typeArguments")
        .size(),
      2,
    )
    assertEquals(
      row.getAsJsonObject("selectedCallable").get("descriptor").getAsString,
      "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;",
    )
  }
  test("shadow-owner-is-not-standard-library") {
    val result = request(
      "class Demo { static class Map { void put(String k, String v) {} } void run(Map map) { map.put(\"k\",\"v\"); } }",
      List("map.put(\"k\",\"v\")"),
    )
    val selected = rows(result).head.getAsJsonObject("selectedCallable")
    assertEquals(selected.get("owner").getAsString, "Demo.Map")
    assertEquals(selected.get("binaryOwner").getAsString, "Demo$Map")
    assertEquals(
      selected.get("descriptor").getAsString,
      "(Ljava/lang/String;Ljava/lang/String;)V",
    )
  }
  test("unsupported-receiver-does-not-borrow-selected-owner") {
    val result = request(
      "class Demo { void run(java.util.List<?> xs) { xs.get(0); } }",
      List("xs.get(0)"),
    )
    val row = rows(result).head
    assertEquals(row.get("status").getAsString, "UNSUPPORTED_TYPE")
    assert(!row.has("receiverType"))
    assertEquals(
      row.getAsJsonObject("selectedCallable").get("owner").getAsString,
      "java.util.List",
    )
  }
  test("project-release-rejects-provider-only-api") {
    val result = request(
      "class Demo { void run(java.util.List<String> xs) { xs.getFirst(); } }",
      List("xs.getFirst()"),
    )
    assertEquals(rows(result).head.get("status").getAsString, "COMPILER_ERRORS")
    assert(!rows(result).head.has("receiverType"))
  }
}
