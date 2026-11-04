package pc

import java.net.URI
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters._

import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.VirtualFileParams

import com.google.gson.JsonObject
import tests.pc.BaseJavaPCSuite

class JavaReceiverTypesSuite extends BaseJavaPCSuite {
  override protected def javacOptions: List[String] =
    List("--release", "17", "-proc:none")

  private def document(source: String): JsonObject =
    batch(Map("Demo.java" -> source))
      .getAsJsonArray("documents")
      .get(0)
      .getAsJsonObject

  private def batch(files: Map[String, String]): JsonObject = {
    val params: List[VirtualFileParams] =
      files.toList.sortBy(_._1).map { case (path, text) =>
        CompilerVirtualFileParams(URI.create(s"file:///$path"), text)
      }
    request(params)
  }

  private def request(params: List[VirtualFileParams]): JsonObject = {
    val response = presentationCompiler
      .receiverTypesExperimental(params.asJava)
      .get(30, TimeUnit.SECONDS)
    assertEquals(
      response.get("schemaVersion").getAsString,
      "metals-java-receiver-types.experimental.v1",
    )
    response
  }

  private def puts(doc: JsonObject): List[JsonObject] =
    doc
      .getAsJsonArray("receiverSites")
      .asScala
      .map(_.getAsJsonObject)
      .filter(_.get("callName").getAsString == "put")
      .toList

  private def nominal(row: JsonObject): JsonObject = {
    assertEquals(row.get("status").getAsString, "TYPED")
    val receiver = row.getAsJsonObject("receiverType")
    assertEquals(receiver.get("kind").getAsString, "DECLARED")
    receiver
  }

  test("parameter-and-generic-chain-use-callsite-type") {
    val source = """package fixture; import java.util.Map;
                   |class Demo {
                   |  <T> T id(T value) { return value; }
                   |  void run(Map<String,String> m) {
                   |    m.put("k","v");
                   |    id(m).put("k","v");
                   |  }
                   |}
                   |""".stripMargin
    val doc = document(source)
    assertEquals(doc.get("status").getAsString, "COMPLETE")
    val rows = puts(doc)
    assertEquals(rows.size, 2)
    val types = rows.map(nominal)
    assertEquals(types.head, types(1))
    assertEquals(types.head.get("qualifiedName").getAsString, "java.util.Map")
    assertEquals(
      types.head
        .getAsJsonArray("typeArguments")
        .get(0)
        .getAsJsonObject
        .get("qualifiedName")
        .getAsString,
      "java.lang.String",
    )
    assertEquals(document(source), doc)
  }

  test("block-local-shadowing-preserves-distinct-types") {
    val doc = document(
      """package fixture; import java.util.Map; import java.util.HashMap;
        |class Demo { void run() {
        |  { Map<String,String> m = new HashMap<>(); m.put("k","v"); }
        |  { Map<Integer,Integer> m = new HashMap<>(); m.put(1,2); }
        |}}
        |""".stripMargin
    )
    val rows = puts(doc)
    assertEquals(rows.size, 2)
    val arguments = rows.map(row =>
      nominal(row)
        .getAsJsonArray("typeArguments")
        .get(0)
        .getAsJsonObject
        .get("qualifiedName")
        .getAsString
    )
    assertEquals(arguments, List("java.lang.String", "java.lang.Integer"))
  }

  test("inherited-member-type-is-not-the-imported-jdk-type") {
    val doc =
      document("""package fixture; import java.util.Map;
                 |class Parent { interface Map<K,V> { V put(K key, V value); } }
                 |class Demo extends Parent {
                 |  void run(Map<String,String> m) { m.put("k","v"); }
                 |}
                 |""".stripMargin)
    val rows = puts(doc)
    assertEquals(rows.size, 1)
    assertEquals(
      nominal(rows.head).get("qualifiedName").getAsString,
      "fixture.Parent.Map",
    )
  }

  test("type-variable-is-not-replaced-with-its-bound") {
    val doc = document(
      """package fixture;
        |class Demo {
        | <M extends java.util.Map<String,String>> void run(M m) { m.put("k","v"); }
        |}
        |""".stripMargin
    )
    val rows = puts(doc)
    assertEquals(rows.size, 1)
    assertEquals(rows.head.get("status").getAsString, "UNSUPPORTED_TYPE")
    assert(!rows.head.has("receiverType"))
  }

  test("compiler-errors-cannot-produce-typed-receivers") {
    val doc = document(
      """class Demo { void run() { missing.put("k","v"); } }"""
    )
    assertEquals(doc.get("status").getAsString, "COMPILER_ERRORS")
    assert(doc.get("compilerErrorCount").getAsInt > 0)
    assertEquals(puts(doc).size, 0)
    val diagnostics = doc.getAsJsonObject("compilerDiagnostics")
    assertEquals(
      diagnostics.get("schemaVersion").getAsString,
      "metals-java-compiler-diagnostics.v1",
    )
    assertEquals(diagnostics.get("totalErrors"), doc.get("compilerErrorCount"))
    assertEquals(diagnostics.get("omittedErrors").getAsInt, 0)
    val error = diagnostics.getAsJsonArray("errors").get(0).getAsJsonObject
    assert(
      error.get("code").getAsString.startsWith("compiler.err.cant.resolve")
    )
    assertEquals(error.get("sourceUri").getAsString, "file:///Demo.java")
    assert(!error.has("message"))
    assert(!diagnostics.toString.contains("missing.put"))
  }

  test("compiler-error-catalog-is-bounded-with-explicit-omissions") {
    val statements =
      (0 until 80).map(i => s"missing$i.put(\"k\",\"v\");").mkString
    val doc = document(s"class Demo { void run() { $statements } }")
    assertEquals(doc.get("status").getAsString, "COMPILER_ERRORS")
    assertEquals(puts(doc).size, 0)
    val diagnostics = doc.getAsJsonObject("compilerDiagnostics")
    val retained = diagnostics.getAsJsonArray("errors").size
    val omitted = diagnostics.get("omittedErrors").getAsInt
    assertEquals(retained, 32)
    assert(omitted > 0)
    assertEquals(retained + omitted, doc.get("compilerErrorCount").getAsInt)
    assert(
      diagnostics.toString
        .getBytes(java.nio.charset.StandardCharsets.UTF_8)
        .length < 33 * 1024
    )
  }

  test("dependency-source-error-remains-task-wide-with-its-own-location") {
    val result = batch(
      Map(
        "Api.java" -> "class Api { String value() { return missing; } }",
        "Caller.java" -> "class Caller { void run(Api api) { api.value().length(); } }",
      )
    )
    val docs = result.getAsJsonArray("documents").asScala.map(_.getAsJsonObject)
    val caller =
      docs.find(_.get("uri").getAsString.endsWith("/Caller.java")).get
    assertEquals(caller.get("status").getAsString, "COMPILER_ERRORS")
    assertEquals(caller.getAsJsonArray("receiverSites").size, 0)
    val diagnostics = caller.getAsJsonObject("compilerDiagnostics")
    assertEquals(diagnostics.get("totalErrors").getAsInt, 1)
    val error = diagnostics.getAsJsonArray("errors").get(0).getAsJsonObject
    assertEquals(error.get("sourceUri").getAsString, "file:///Api.java")
    assert(!error.has("message"))
  }

  test("cross-file-return-is-analyzed-in-the-same-batch") {
    val result = batch(
      Map(
        "Api.java" -> """package fixture; import java.util.Map;
                        |class Api { Map<String,String> values() { return null; } }
                        |""".stripMargin,
        "Demo.java" -> """package fixture;
                         |class Demo { void run(Api api) { api.values().put("k","v"); } }
                         |""".stripMargin,
      )
    )
    val docs = result
      .getAsJsonArray("documents")
      .asScala
      .map(_.getAsJsonObject)
    val caller = docs.find(_.get("uri").getAsString.endsWith("/Demo.java")).get
    assertEquals(puts(caller).size, 1)
    assertEquals(
      nominal(puts(caller).head).get("qualifiedName").getAsString,
      "java.util.Map",
    )
  }

  test("utf16-ranges-and-source-identity-are-exact") {
    val source =
      """package fixture; import java.util.Map;
        |// supplementary character: 😀
        |class Demo { void run(Map<String,String> m) { m.put("k","v"); } }
        |""".stripMargin
    val doc = document(source)
    val row = puts(doc).head
    assertEquals(
      row.getAsJsonObject("callRange").get("startUtf16").getAsInt,
      source.indexOf("m.put"),
    )
    assertEquals(
      row.getAsJsonObject("receiverRange").get("endUtf16").getAsInt,
      source.indexOf("m.put") + 1,
    )
    assertNotEquals(
      doc.get("sourceSha256"),
      document("// prefix\n" + source).get("sourceSha256"),
    )
  }

  test("array-parenthesized-and-field-receivers-use-expression-types") {
    val doc =
      document("""package fixture; import java.util.Map;
                 |class Demo {
                 | Map<String,String> field;
                 | void run(Map<String,String>[] maps, Map<String,String> map) {
                 |   maps[0].put("k","v");
                 |   (map).put("k","v");
                 |   this.field.put("k","v");
                 | }
                 |}
                 |""".stripMargin)
    val rows = puts(doc)
    assertEquals(rows.size, 3)
    assertEquals(
      rows.map(_.get("receiverSyntax").getAsString),
      List("ARRAY_ACCESS", "PARENTHESIZED", "MEMBER_SELECT"),
    )
    rows.foreach { row =>
      assertEquals(row.get("receiverRole").getAsString, "VALUE")
      assertEquals(
        nominal(row).get("qualifiedName").getAsString,
        "java.util.Map",
      )
    }
  }

  test("wildcard-arguments-do-not-become-nominal-type-arguments") {
    val doc =
      document("""import java.util.Map;
                 |class Demo { void run(Map<String,?> m) { m.put("k",null); } }
                 |""".stripMargin)
    assertEquals(puts(doc).size, 1)
    assertEquals(puts(doc).head.get("status").getAsString, "UNSUPPORTED_TYPE")
  }

  test("static-type-qualifier-is-not-an-instance-receiver") {
    val doc =
      document("""class Demo {
                 | void run() { java.util.Objects.requireNonNull("value"); }
                 |}
                 |""".stripMargin)
    val rows = doc
      .getAsJsonArray("receiverSites")
      .asScala
      .map(_.getAsJsonObject)
    val call = rows.find(_.get("callName").getAsString == "requireNonNull").get
    assertEquals(call.get("receiverRole").getAsString, "TYPE_QUALIFIER")
  }

  test("site-budget-retains-explicit-omission-count") {
    val calls = List.fill(257)("m.put(1,2);").mkString
    val source =
      s"import java.util.Map; class Demo { void run(Map<Integer,Integer> m) { $calls } }"
    val doc = document(source)
    assertEquals(doc.get("status").getAsString, "PARTIAL")
    assertEquals(puts(doc).size, 256)
    assertEquals(doc.get("omittedReceiverSites").getAsInt, 1)
  }

  private def assertUnavailable(result: JsonObject, count: Int): Unit = {
    assertEquals(
      result.get("status").getAsString,
      "INPUT_LIMIT_OR_DUPLICATE_URI",
    )
    assertEquals(result.get("requestedDocumentCount").getAsInt, count)
    assertEquals(result.getAsJsonArray("documents").size(), 0)
  }

  test("duplicate-uri-cannot-select-one-source-version") {
    val uri = URI.create("file:///Demo.java")
    assertUnavailable(
      request(
        List(
          CompilerVirtualFileParams(uri, "class Demo {}"),
          CompilerVirtualFileParams(uri, "class Demo { int different; }"),
        )
      ),
      2,
    )
  }

  test("file-count-limit-is-explicit") {
    val files = (1 to 17).map(i => s"Demo$i.java" -> s"class Demo$i {}").toMap
    assertUnavailable(batch(files), 17)
  }

  test("source-byte-limit-counts-utf8-not-utf16") {
    val source = "//" + "\u00e9" * (512 * 1024)
    assert(source.length < 1024 * 1024)
    assertUnavailable(batch(Map("Demo.java" -> source)), 1)
  }

  test("batch-byte-limit-is-explicit") {
    val source = "//" + "x" * (900 * 1024)
    val files = (1 to 5).map(i => s"Demo$i.java" -> source).toMap
    assertUnavailable(batch(files), 5)
  }

  test("sibling-compiler-error-withholds-the-entire-batch") {
    val result = batch(
      Map(
        "Demo.java" ->
          "class Demo { void run(java.util.Map<String,String> m) { m.put(\"k\",\"v\"); } }",
        "Broken.java" -> "class Broken { MissingType missing; }",
      )
    )
    val documents = result.getAsJsonArray("documents").asScala
    assertEquals(documents.size, 2)
    documents.foreach { value =>
      val doc = value.getAsJsonObject
      assertEquals(doc.get("status").getAsString, "COMPILER_ERRORS")
      assert(doc.get("compilerErrorCount").getAsInt > 0)
      assertEquals(doc.getAsJsonArray("receiverSites").size(), 0)
    }
  }

  test("nonstatic-enclosing-type-is-not-silently-erased") {
    val doc = document("""class Demo<T> {
                         |  class Inner { void use() {} }
                         |  void run(Inner value) { value.use(); }
                         |}
                         |""".stripMargin)
    assertEquals(doc.get("status").getAsString, "COMPLETE")
    val rows = doc.getAsJsonArray("receiverSites")
    assertEquals(rows.size(), 1)
    assertEquals(
      rows.get(0).getAsJsonObject.get("status").getAsString,
      "UNSUPPORTED_TYPE",
    )
  }
}
