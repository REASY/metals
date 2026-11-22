package tests

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

import scala.meta.internal.metals.JavaReceiverRange
import scala.meta.internal.metals.JavaReceiverTypes
import scala.meta.internal.metals.JavaReceiverTypesParams
import scala.meta.internal.metals.MetalsEnrichments._

import com.google.gson.Gson
import com.google.gson.JsonObject

class JavaReceiverLspSuite extends BaseLspSuite("java-receiver") {
  private def hash(text: String): String = "sha256:" + MessageDigest
    .getInstance("SHA-256")
    .digest(text.getBytes(UTF_8))
    .map(b => f"${b & 0xff}%02x")
    .mkString

  testLSP("mixed-target-cross-file-receiver-and-project-release") {
    cleanWorkspace()
    val file = "a/src/main/java/fixture/Caller.java"
    val source =
      "package fixture; class Caller { <T> T id(T x) { return x; } void run() { id(Api.value()).put(\"k\",\"v\"); } }"
    val expression = "id(Api.value()).put(\"k\",\"v\")"
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18","javacOptions":["--release","17"]}}
            |/a/src/main/java/fixture/Api.java
            |package fixture; class Api { static java.util.Map<String,String> value() { return null; } }
            |/a/src/main/scala/fixture/Marker.scala
            |package fixture
            |object Marker
            |/$file
            |$source
            |""".stripMargin
      )
      _ <- server.didOpenAndFocus(file)
      path = server.toPath(file)
      target = server.server.buildTargets.inverseSources(path).get
      evaluatedOptions = server.server.buildTargets
        .javaTarget(target)
        .get
        .options
      text = path.readText
      start = text.indexOf(expression)
      params = JavaReceiverTypesParams(
        path.toURI.toString,
        target.getUri,
        hash(text),
        List(JavaReceiverRange(start, start + expression.length)).asJava,
      )
      result <- server.fullServer.javaReceiverTypes(params).asScala
      repeat <- server.fullServer.javaReceiverTypes(params).asScala
      stale <- server.fullServer
        .javaReceiverTypes(params.copy(sourceSha256 = hash(text + " ")))
        .asScala
      wrongTarget <- server.fullServer
        .javaReceiverTypes(
          params.copy(buildTargetId = "file:///not-this-target")
        )
        .asScala
    } yield {
      assertEquals(
        result.get("status").getAsString,
        "DOCUMENT_RESULTS",
        clues(
          result,
          server.server.buildTargets.javaTarget(target).map(_.options),
        ),
      )
      assertEquals(
        result.get("schemaVersion").getAsString,
        "metals-java-receiver-types.v2",
      )
      assertEquals(result.get("javaRelease").getAsString, "17")
      assertEquals(
        result.get("compilerOptionsSha256").getAsString,
        JavaReceiverTypes.optionsHash(List("--release", "17")),
      )
      assertEquals(result.get("buildTargetId").getAsString, target.getUri)
      val doc = result.getAsJsonArray("documents").get(0).getAsJsonObject
      assertEquals(doc.get("compilerErrorCount").getAsInt, 0)
      val row = doc.getAsJsonArray("receiverSites").get(0).getAsJsonObject
      assertEquals(
        row.getAsJsonObject("receiverType").get("qualifiedName").getAsString,
        "java.util.Map",
      )
      assertEquals(
        row.getAsJsonObject("selectedCallable").get("owner").getAsString,
        "java.util.Map",
      )
      assertEquals(result, repeat)
      assertEquals(stale.get("status").getAsString, "TARGET_SOURCE_MISMATCH")
      assertEquals(
        wrongTarget.get("status").getAsString,
        "BUILD_TARGET_MISMATCH",
      )
      assertEquals(path.readText, text)
      assertEquals(
        result.getAsJsonArray("evaluatedCompilerOptions"),
        new Gson().toJsonTree(evaluatedOptions.asJava).getAsJsonArray,
      )
      // Opt-in public interoperability artifact for the separately built
      // consumer. Never log source or overwrite a previous measurement.
      sys.env.get("CODEHOP_JAVA_RECEIVER_FIXTURE_OUT").foreach { output =>
        val artifact = new JsonObject()
        artifact.addProperty("format", "metals-java-receiver-public-fixture.v1")
        artifact.addProperty("source", text)
        artifact.addProperty("file", file)
        artifact.add("params", new Gson().toJsonTree(params))
        artifact.add(
          "evaluatedOptions",
          new Gson().toJsonTree(evaluatedOptions.asJava),
        )
        artifact.add("result", result)
        java.nio.file.Files.writeString(
          java.nio.file.Paths.get(output),
          artifact.toString,
          java.nio.file.StandardOpenOption.CREATE_NEW,
        )
      }
    }
  }

  testLSP("project-release-excludes-runtime-only-api") {
    cleanWorkspace()
    val file = "a/src/main/java/Caller.java"
    val source =
      "class Caller { String run(java.util.List<String> xs) { return xs.getFirst(); } }"
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18","javacOptions":["--release","17"]}}
            |/$file
            |$source
            |""".stripMargin
      )
      _ <- server.didOpenAndFocus(file)
      path = server.toPath(file)
      text = path.readText
      target = server.server.buildTargets.inverseSources(path).get
      start = text.indexOf("xs.getFirst()")
      result <- server.fullServer
        .javaReceiverTypes(
          JavaReceiverTypesParams(
            path.toURI.toString,
            target.getUri,
            hash(text),
            List(
              JavaReceiverRange(start, start + "xs.getFirst()".length)
            ).asJava,
          )
        )
        .asScala
    } yield {
      assertEquals(
        result.get("status").getAsString,
        "DOCUMENT_RESULTS",
        clues(result),
      )
      assertEquals(result.get("javaRelease").getAsString, "17")
      val doc = result.getAsJsonArray("documents").get(0).getAsJsonObject
      assert(doc.get("compilerErrorCount").getAsInt > 0)
      val row = doc.getAsJsonArray("receiverSites").get(0).getAsJsonObject
      assertEquals(row.get("status").getAsString, "COMPILER_ERRORS")
      assert(!row.has("receiverType"))
      assertEquals(path.readText, text)
    }
  }

  test("invalid-request-is-rejected-before-source-access") {
    val good = JavaReceiverTypesParams(
      "file:///Caller.java",
      "target",
      "sha256:" + "0" * 64,
      List(JavaReceiverRange(0, 1)).asJava,
    )
    assert(JavaReceiverTypes.valid(good))
    for (
      uri <- List("file:opaque", "https://example.com/A.java",
        "file://remote/A.java", "file:///A.java?query", "file:///A.scala")
    )
      assert(!JavaReceiverTypes.valid(good.copy(uri = uri)))
    assert(
      !JavaReceiverTypes.valid(
        good.copy(targets = List.fill(257)(JavaReceiverRange(0, 1)).asJava)
      )
    )
    assert(
      !JavaReceiverTypes.valid(
        good.copy(targets = List[JavaReceiverRange](null).asJava)
      )
    )
    assert(
      !JavaReceiverTypes.valid(
        good.copy(targets = List(JavaReceiverRange(-1, 1)).asJava)
      )
    )
    assert(!JavaReceiverTypes.valid(good.copy(sourceSha256 = "stale")))
    assert(!JavaReceiverTypes.valid(null))
  }

  test("bsp-instrumentation-options-preserve-project-semantics") {
    val semantics = List("--release", "17", "-parameters", "-Xlint:all")
    val instrumentation = List(
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
      "-Xplugin:semanticdb -sourceroot:/fixture -targetroot:javac-classes-directory",
    )
    assertEquals(
      JavaReceiverTypes.compilerOptions(instrumentation ++ semantics),
      Right(semantics),
    )
    assertEquals(JavaReceiverTypes.compilerOptions(semantics), Right(semantics))
    for (
      unknown <- List(
        List("-J--add-exports"),
        List("-J-Xmx1g"),
        List("-J--add-exports", "-Jjava.base/java.lang=ALL-UNNAMED"),
      )
    )
      assert(JavaReceiverTypes.compilerOptions(unknown).isLeft)
    // A similarly named plugin is not the recognized instrumentation.
    val custom = List("-Xplugin:semanticdb-custom")
    assertEquals(JavaReceiverTypes.compilerOptions(custom), Right(custom))
    assertNotEquals(
      JavaReceiverTypes.optionsHash(instrumentation ++ semantics),
      JavaReceiverTypes.optionsHash(semantics),
    )
  }
}
