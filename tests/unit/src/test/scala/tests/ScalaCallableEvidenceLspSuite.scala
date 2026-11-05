package tests

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.SemanticdbTextDocumentsParams

class ScalaCallableEvidenceLspSuite
    extends BaseLspSuite("scala-callable-quoted-owner") {
  testLSP("quoted-cross-file-generated-and-authored-selections-stay-distinct") {
    cleanWorkspace()
    val owner = "a/src/main/scala/fixture/Headers.scala"
    val caller = "a/src/main/scala/fixture/Caller.scala"
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18"}}
            |/$owner
            |package fixture
            |final case class `Good-Entry`(values: Seq[String])
            |object `Good-Entry` {
            |  def apply(value: String): `Good-Entry` = new `Good-Entry`(Seq(value))
            |}
            |final case class `Other-Entry`(allow: Boolean)
            |/$caller
            |package fixture
            |object Caller {
            |  val generated = `Good-Entry`(Seq("x"))
            |  val authored = `Good-Entry`("x")
            |  val qualified = fixture.`Good-Entry`(Seq("y"))
            |  val other = `Other-Entry`(true)
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
        .scalaCallableEvidence(
          SemanticdbTextDocumentsParams(uris.asJava)
        )
        .asScala
      reversed <- server.fullServer
        .scalaCallableEvidence(
          SemanticdbTextDocumentsParams(uris.reverse.asJava)
        )
        .asScala
      _ <- server.didOpen(owner)
      reloaded <- server.fullServer
        .scalaCallableEvidence(
          SemanticdbTextDocumentsParams(uris.asJava)
        )
        .asScala
    } yield {
      sys.env.get("CODEHOP_QUOTED_CALLABLE_FIXTURE_OUT").foreach { output =>
        java.nio.file.Files.writeString(
          java.nio.file.Paths.get(output),
          new com.google.gson.Gson().toJson(result),
          java.nio.file.StandardOpenOption.CREATE_NEW,
        )
      }
      assertEquals(result.status, "AVAILABLE")
      assertEquals(result.schemaVersion, "metals-scala-callable-evidence.v2")
      val documents = result.documents.asScala.toSeq
      assertEquals(documents.size, 2)
      assert(
        documents.forall(_.status == "AVAILABLE"),
        clues(documents.map(d => (d.status, d.reason))),
      )
      assert(documents.forall(_.md5.nonEmpty))
      val callerDocument =
        documents.find(_.uri == callerPath.toURI.toString).get
      val selections = callerDocument.selections.asScala
        .filter(_.symbol.startsWith("fixture/`"))
        .sortBy(_.range.getStart.getLine)
      assertEquals(selections.size, 4)
      assertEquals(selections(0).symbol, selections(2).symbol)
      assertNotEquals(selections(0).symbol, selections(1).symbol)
      assert(selections(0).symbol.startsWith("fixture/`Good-Entry`.apply("))
      assert(selections(3).symbol.startsWith("fixture/`Other-Entry`.apply("))
      val signatures = documents.flatMap(_.callables.asScala)
      for (selection <- selections) {
        val matching = signatures.filter(_.symbol == selection.symbol)
        assertEquals(matching.size, 1)
        val signature = matching.head
        assertEquals(signature.status, "AVAILABLE")
        assertEquals(signature.parameterLists.size, 1)
        assertEquals(signature.parameterLists.get(0).size, 1)
        assert(
          signature.returnType.get("symbol").getAsString.startsWith("fixture/`")
        )
      }
      val generated = signatures.find(_.symbol == selections(0).symbol).get
      val authored = signatures.find(_.symbol == selections(1).symbol).get
      assertEquals(generated.parameterLists.get(0).get(0).displayName, "values")
      assertEquals(authored.parameterLists.get(0).get(0).displayName, "value")
      val reverseSelections = reversed.documents.asScala
        .find(_.uri == callerPath.toURI.toString)
        .get
        .selections
        .asScala
        .map(_.symbol)
        .toSet
      assertEquals(
        reverseSelections,
        callerDocument.selections.asScala.map(_.symbol).toSet,
      )
      assertEquals(reloaded, result)
      assertEquals(callerPath.readText, callerBefore)
      assertEquals(ownerPath.readText, ownerBefore)
    }
  }

}
