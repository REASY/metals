package tests

import scala.meta.internal.metals.MetalsEnrichments.*
import scala.meta.internal.metals.SemanticdbTextDocumentsParams

class ScalaConstructorEvidenceLspSuite
    extends BaseLspSuite("scala-constructor-evidence") {
  testLSP("cross-file-overloads-alias-and-source-identity") {
    cleanWorkspace()
    val owner = "a/src/main/scala/fixture/Choice.scala"
    val caller = "a/src/main/scala/fixture/Caller.scala"
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18"}}
            |/$owner
            |package fixture
            |class Choice(value: Int) {
            |  def this(value: String) = this(value.length)
            |}
            |trait Marker
            |/$caller
            |package fixture
            |object Caller {
            |  /* 😀 */ val number = new Choice(1)
            |  val text = new Choice("x")
            |  val alias = new RuntimeException("fixture")
            |  val anonymous = new Choice(2) with Marker { val initialized = 3 }
            |}
            |""".stripMargin
      )
      _ <- server.didOpenAndFocus(caller)
      callerPath = server.toPath(caller)
      ownerPath = server.toPath(owner)
      callerBefore = callerPath.readText
      ownerBefore = ownerPath.readText
      uris = List(callerPath.toURI.toString, ownerPath.toURI.toString)
      result <- server.fullServer
        .scalaConstructorEvidence(
          SemanticdbTextDocumentsParams(uris.asJava)
        )
        .asScala
      reversed <- server.fullServer
        .scalaConstructorEvidence(
          SemanticdbTextDocumentsParams(uris.reverse.asJava)
        )
        .asScala
    } yield {
      sys.env.get("CODEHOP_CONSTRUCTOR_FIXTURE_OUT").foreach { output =>
        java.nio.file.Files.writeString(
          java.nio.file.Paths.get(output),
          new com.google.gson.Gson().toJson(result),
          java.nio.file.StandardOpenOption.CREATE_NEW,
        )
      }
      assertEquals(result.status, "AVAILABLE", clues(result.reason))
      assertEquals(result.schemaVersion, "metals-scala-constructor-evidence.v2")
      val documents = result.documents.asScala.toSeq
      assertEquals(documents.size, 2)
      assert(
        documents.forall(_.status == "AVAILABLE"),
        clues(documents.map(_.reason)),
      )
      assert(documents.forall(_.sourceMd5.nonEmpty))
      val rows = documents
        .find(_.uri == callerPath.toURI.toString)
        .get
        .invocations
        .asScala
        .toSeq
      assertEquals(rows.size, 4)
      assert(rows.forall(_.status == "AVAILABLE"), clues(rows.map(_.reason)))
      assertNotEquals(rows(0).selectedSymbol, rows(1).selectedSymbol)
      assertEquals(rows(0).declaringOwnerSymbol, "fixture/Choice#")
      assertEquals(rows(1).declaringOwnerSymbol, "fixture/Choice#")
      assertEquals(rows(2).declaringOwnerSymbol, "java/lang/RuntimeException#")
      val anonymous = rows(3).anonymousConstructor
      assert(anonymous != null)
      assertEquals(anonymous.initializerKind, "ANONYMOUS_CLASS_INITIALIZATION")
      assertEquals(anonymous.ownerRange, rows(3).invocationRange)
      assert(anonymous.parentSymbols.asScala.contains("fixture/Choice#"))
      assert(anonymous.parentSymbols.asScala.contains("fixture/Marker#"))
      assertEquals(rows(3).selectedSymbol, null)
      val json = new com.google.gson.Gson().toJsonTree(result).toString
      assert(json.contains("anonymousConstructor"))
      assert(json.contains("TYPED_ALLOCATION_BEFORE_LAMBDA_LIFT"))
      assertEquals(
        reversed.documents.asScala
          .find(_.uri == callerPath.toURI.toString)
          .get
          .invocations
          .asScala
          .map(_.selectedSymbol)
          .toSeq,
        rows.map(_.selectedSymbol),
      )
      assertEquals(callerPath.readText, callerBefore)
      assertEquals(ownerPath.readText, ownerBefore)
    }
  }

  testLSP("invalid-document-bound-abstains") {
    for {
      _ <- initialize("")
      result <- server.fullServer
        .scalaConstructorEvidence(
          SemanticdbTextDocumentsParams(List.empty[String].asJava)
        )
        .asScala
      oversized <- server.fullServer
        .scalaConstructorEvidence(
          SemanticdbTextDocumentsParams(
            (0 until 17).map(i => s"file:///fixture/$i.scala").asJava
          )
        )
        .asScala
    } yield {
      assertEquals(result.status, "UNAVAILABLE")
      assertEquals(result.reason, "INVALID_REQUEST")
      assertEquals(oversized.status, "UNAVAILABLE")
      assertEquals(oversized.reason, "INVALID_REQUEST")
      assert(result.documents.isEmpty && oversized.documents.isEmpty)
    }
  }
}
