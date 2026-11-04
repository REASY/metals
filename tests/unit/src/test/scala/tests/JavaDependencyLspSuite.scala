package tests

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

import scala.meta.internal.metals.JavaDependencyBodies
import scala.meta.internal.metals.JavaDependencyBodiesParams
import scala.meta.internal.metals.JavaReceiverRange
import scala.meta.internal.metals.JavaReceiverTypes
import scala.meta.internal.metals.MetalsEnrichments._

import com.google.gson.Gson
import com.google.gson.JsonObject

class JavaDependencyLspSuite extends BaseLspSuite("java-dependency-bodies") {
  private def hash(text: String): String = "sha256:" + MessageDigest
    .getInstance("SHA-256")
    .digest(text.getBytes(UTF_8))
    .map(b => f"${b & 0xff}%02x")
    .mkString

  testLSP(
    "binary-dependency-bodies-through-service-preserve-cross-file-target"
  ) {
    cleanWorkspace()
    val file = "a/src/main/java/fixture/Caller.java"
    val source =
      """package fixture; class Caller { void run() { Api.logger().warn("hello"); } }"""
    val expression = """Api.logger().warn("hello")"""
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18","javacOptions":["--release","17"],"libraryDependencies":["org.slf4j:slf4j-api:2.0.17"]}}
            |/a/src/main/java/fixture/Api.java
            |package fixture; class Api { static org.slf4j.Logger logger() { return null; } }
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
      frozenClasspath <- server.server.buildTargets
        .targetClasspath(target, scala.concurrent.Promise[Unit]())
        .get
      frozenPaths = frozenClasspath.toAbsoluteClasspath
        .map(_.toNIO.toAbsolutePath.normalize().toString)
        .toList
      text = path.readText
      start = text.indexOf(expression)
      freezeParams = JavaDependencyBodiesParams(
        path.toURI.toString,
        target.getUri,
        hash(text),
        List(JavaReceiverRange(start, start + expression.length)).asJava,
        262144,
        null,
        "FREEZE",
      )
      frozen <- server.fullServer.javaDependencyBodies(freezeParams).asScala
      params = freezeParams.copy(
        phase = "ACQUIRE",
        compilerClasspathSha256 = JavaReceiverTypes.optionsHash(frozenPaths),
        compilerContentSha256 = frozen.get("compilerContentSha256").getAsString,
      )
      result <- server.fullServer.javaDependencyBodies(params).asScala
      repeat <- server.fullServer.javaDependencyBodies(params).asScala
      stale <- server.fullServer
        .javaDependencyBodies(params.copy(sourceSha256 = hash(text + " ")))
        .asScala
      wrong <- server.fullServer
        .javaDependencyBodies(params.copy(buildTargetId = "file:///other"))
        .asScala
      invalid <- server.fullServer
        .javaDependencyBodies(params.copy(maxInventoryEntries = 262145))
        .asScala
      changed <- server.fullServer
        .javaDependencyBodies(
          params.copy(compilerClasspathSha256 = hash("changed"))
        )
        .asScala
      missing <- server.fullServer
        .javaDependencyBodies(params.copy(compilerClasspathSha256 = null))
        .asScala
      staleContent <- server.fullServer
        .javaDependencyBodies(
          params.copy(compilerContentSha256 = hash("changed"))
        )
        .asScala
    } yield {
      assertEquals(
        frozen.get("status").getAsString,
        "CONTEXT_READY",
        clues(frozen),
      )
      assertEquals(
        result.get("schemaVersion").getAsString,
        JavaDependencyBodies.SchemaVersion,
      )
      assertEquals(
        result.get("status").getAsString,
        "DOCUMENT_RESULTS",
        clues(result),
      )
      assertEquals(result.get("buildTargetId").getAsString, target.getUri)
      assertEquals(result.get("javaRelease").getAsString, "17")
      assertEquals(
        result.get("compilerOptionsSha256").getAsString,
        JavaReceiverTypes.optionsHash(List("--release", "17")),
      )
      val cp =
        result.getAsJsonArray("compilerClasspath").asScala.map(_.getAsString)
      assertEquals(cp.toSeq, frozenPaths)
      assert(cp.exists(_.endsWith("slf4j-api-2.0.17.jar")))
      assertEquals(
        result.get("compilerClasspathSha256").getAsString,
        JavaReceiverTypes.optionsHash(cp.toList),
      )
      val doc = result.getAsJsonArray("documents").get(0).getAsJsonObject
      val row = doc.getAsJsonArray("receiverSites").get(0).getAsJsonObject
      assertEquals(
        row.getAsJsonObject("selectedCallable").get("owner").getAsString,
        "org.slf4j.Logger",
      )
      val found = row.getAsJsonObject("dependencyBodies")
      assertEquals(found.get("status").getAsString, "SCANNED")
      assert(!found.get("populationClosed").getAsBoolean)
      assert(
        found
          .getAsJsonArray("bodies")
          .asScala
          .exists(
            _.getAsJsonObject
              .get("owner")
              .getAsString == "org.slf4j.helpers.NOPLogger"
          )
      )
      assertEquals(result, repeat)
      assertEquals(stale.get("status").getAsString, "TARGET_SOURCE_MISMATCH")
      assertEquals(wrong.get("status").getAsString, "BUILD_TARGET_MISMATCH")
      assertEquals(invalid.get("status").getAsString, "INVALID_REQUEST")
      assertEquals(changed.get("status").getAsString, "CLASSPATH_CHANGED")
      assertEquals(missing.get("status").getAsString, "INVALID_REQUEST")
      assertEquals(staleContent.get("status").getAsString, "CONTEXT_CHANGED")
      assertEquals(
        result.get("compilerContentSha256"),
        frozen.get("compilerContentSha256"),
      )
      assertEquals(path.readText, text)
      sys.env.get("CODEHOP_JAVA_DEPENDENCY_FIXTURE_OUT").foreach { output =>
        val artifact = new JsonObject()
        artifact.addProperty(
          "format",
          "metals-java-dependency-public-fixture.v1",
        )
        artifact.addProperty("source", text)
        artifact.addProperty("file", file)
        artifact.add("params", new Gson().toJsonTree(params))
        artifact.add("freezeParams", new Gson().toJsonTree(freezeParams))
        artifact.add("frozen", frozen)
        artifact.add(
          "frozenClasspath",
          new Gson().toJsonTree(frozenPaths.asJava),
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
}
