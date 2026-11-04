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

class JavaReceiverTargetsSuite extends BaseJavaPCSuite {
  override protected def javacOptions: List[String] =
    List("--release", "17", "-proc:none")
  private val uri = URI.create("file:///Demo.java")
  private val source =
    "class Demo { void run(java.util.Map<Integer,Integer> m) { m.put(1,2); m.get(3); } }"
  private def hash(text: String): String = "sha256:" + MessageDigest
    .getInstance("SHA-256")
    .digest(text.getBytes(UTF_8))
    .map(b => f"${b & 0xff}%02x")
    .mkString
  private def target(text: String, expression: String): JavaReceiverTarget = {
    val start = text.lastIndexOf(expression)
    assert(start >= 0)
    JavaReceiverTarget(uri, hash(text), start, start + expression.length)
  }
  private def request(
      text: String,
      targets: List[JavaReceiverTarget],
  ): JsonObject =
    batch(List(CompilerVirtualFileParams(uri, text)), targets)
  private def batch(
      params: List[VirtualFileParams],
      targets: List[JavaReceiverTarget],
  ): JsonObject = {
    val result = presentationCompiler
      .receiverTypesAtExperimental(params.asJava, targets.asJava)
      .get(30, TimeUnit.SECONDS)
    assertEquals(
      result.get("schemaVersion").getAsString,
      "metals-java-receiver-types.experimental.targeted.v1",
    )
    result
  }
  private def document(result: JsonObject): JsonObject =
    result.getAsJsonArray("documents").get(0).getAsJsonObject
  private def rows(result: JsonObject): List[JsonObject] = document(result)
    .getAsJsonArray("receiverSites")
    .asScala
    .map(_.getAsJsonObject)
    .toList
  private def rejected(
      targets: List[JavaReceiverTarget],
      status: String,
  ): Unit = {
    val result = request(source, targets)
    assertEquals(result.get("status").getAsString, status)
    assertEquals(result.getAsJsonArray("documents").size(), 0)
  }

  test("unrequested-prefix-does-not-consume-output-budget") {
    val text =
      s"class Demo { void run(java.util.Map<Integer,Integer> m) { ${List.fill(300)("m.get(1);").mkString} m.put(900,901); } }"
    val wanted = target(text, "m.put(900,901)")
    val result = request(text, List(wanted))
    assertEquals(rows(result).size, 1)
    assertEquals(rows(result).head.get("status").getAsString, "TYPED")
    assertEquals(
      rows(result).head.getAsJsonObject("callRange").get("startUtf16").getAsInt,
      wanted.startUtf16,
    )
    assertEquals(document(result).get("requestedReceiverSites").getAsInt, 1)
    assertEquals(document(result).get("omittedReceiverSites").getAsInt, 0)
  }
  test("target-order-is-deterministic") {
    val targets = List(target(source, "m.put(1,2)"), target(source, "m.get(3)"))
    val first = request(source, targets)
    assertEquals(first, request(source, targets.reverse))
    assertEquals(rows(first).size, 2)
  }
  test("unmatched-range-has-an-explicit-terminal") {
    val result =
      request(source, List(JavaReceiverTarget(uri, hash(source), 0, 1)))
    assertEquals(rows(result).size, 1)
    assertEquals(
      rows(result).head.get("status").getAsString,
      "TARGET_NOT_FOUND_OR_UNSUPPORTED_SYNTAX",
    )
  }
  test("stale-source-hash-is-rejected") {
    rejected(
      List(target(source, "m.get(3)").copy(sourceSha256 = hash(source + " "))),
      "TARGET_SOURCE_MISMATCH",
    )
  }
  test("duplicate-target-range-is-rejected") {
    val t = target(source, "m.get(3)")
    rejected(List(t, t), "DUPLICATE_TARGET_RANGE")
  }
  test("target-uri-must-identify-an-input-document") {
    rejected(
      List(
        target(source, "m.get(3)").copy(uri = URI.create("file:///Other.java"))
      ),
      "TARGET_URI_UNAVAILABLE",
    )
  }
  test("negative-empty-and-out-of-source-ranges-are-rejected") {
    for ((start, end) <- List((-1, 1), (0, 0), (0, source.length + 1)))
      rejected(
        List(JavaReceiverTarget(uri, hash(source), start, end)),
        "INVALID_TARGET_RANGE",
      )
  }
  test("target-limit-is-enforced-before-duplicates") {
    rejected(List.fill(257)(target(source, "m.get(3)")), "TARGET_COUNT_LIMIT")
  }
  test("empty-target-request-does-not-mean-whole-document") {
    rejected(Nil, "TARGET_COUNT_LIMIT")
  }

  test("input-document-limit-is-enforced") {
    val params = (0 until 17)
      .map(i =>
        CompilerVirtualFileParams(URI.create(s"file:///File$i.java"), source)
      )
      .toList
    val result = batch(params, List(target(source, "m.get(3)")))
    assertEquals(
      result.get("status").getAsString,
      "INPUT_LIMIT_OR_DUPLICATE_URI",
    )
    assertEquals(result.get("requestedDocumentCount").getAsInt, 17)
  }

  test("batch-target-count-is-bounded") {
    rejected(List.fill(4097)(target(source, "m.get(3)")), "TARGET_COUNT_LIMIT")
  }
  test("utf16-boundaries-and-source-identity-are-exact") {
    val text = "// \uD83D\uDE00\n" + source
    val valid = request(text, List(target(text, "m.get(3)")))
    assertEquals(rows(valid).head.get("status").getAsString, "TYPED")
    val invalid = request(text, List(JavaReceiverTarget(uri, hash(text), 4, 5)))
    assertEquals(invalid.get("status").getAsString, "INVALID_TARGET_RANGE")
  }
  test("compiler-errors-retain-a-terminal-for-every-target") {
    val text = "class Demo { Missing m; void run() { m.put(1,2); m.get(3); } }"
    val result =
      request(text, List(target(text, "m.put(1,2)"), target(text, "m.get(3)")))
    assertEquals(
      rows(result).map(_.get("status").getAsString),
      List("COMPILER_ERRORS", "COMPILER_ERRORS"),
    )
  }
  test("supporting-cross-file-source-needs-no-requested-targets") {
    val text = "class Demo { void run() { Api.value().put(1,2); } }"
    val result = batch(
      List(
        CompilerVirtualFileParams(uri, text),
        CompilerVirtualFileParams(
          URI.create("file:///ZApi.java"),
          "class Api { static java.util.Map<Integer,Integer> value() { return null; } }",
        ),
      ),
      List(target(text, "Api.value().put(1,2)")),
    )
    assertEquals(rows(result).head.get("status").getAsString, "TYPED")
    val support = result.getAsJsonArray("documents").get(1).getAsJsonObject
    assertEquals(support.get("requestedReceiverSites").getAsInt, 0)
    assertEquals(support.getAsJsonArray("receiverSites").size(), 0)
  }
  test("unsupported-type-remains-an-explicit-terminal") {
    val text =
      "class Demo { <M extends java.util.Map<String,String>> void run(M m) { m.put(\"k\",\"v\"); } }"
    val result = request(text, List(target(text, "m.put(\"k\",\"v\")")))
    assertEquals(
      rows(result).head.get("status").getAsString,
      "UNSUPPORTED_TYPE",
    )
  }
  test("oversize-type-row-retains-request-identity-and-budget-terminal") {
    val name = "Long" + "x" * 800
    val parameters = (0 until 24).map(i => s"T$i").mkString(",")
    val arguments = List.fill(24)(name).mkString(",")
    val text =
      s"class Demo { static class $name {} static class Box<$parameters> { void use() {} } void run(Box<$arguments> v) { v.use(); } }"
    val result = request(text, List(target(text, "v.use()")))
    assertEquals(rows(result).size, 1)
    assertEquals(rows(result).head.get("status").getAsString, "OUTPUT_BUDGET")
    assertEquals(document(result).get("omittedReceiverSites").getAsInt, 1)
  }
}
