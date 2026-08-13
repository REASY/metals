package tests.mbt

import scala.jdk.CollectionConverters._

class MbtSemanticdbTextDocumentsSuite
    extends BaseMbtReferenceSuite("mbt-semanticdb-text-documents") {

  testLSP("batch-cross-file-symbol") {
    cleanWorkspace()
    val api = "a/src/main/scala/a/Api.scala"
    val main = "b/src/main/scala/b/Main.scala"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {},
           |  "b": {"dependsOn": ["a"]}
           |}
           |/$api
           |package a
           |object Api {
           |  def answer(): Int = 42
           |}
           |/$main
           |package b
           |object Main {
           |  val result = a.Api.answer()
           |}
           |""".stripMargin
      )
      _ <- server.didOpenAndFocus(main)
      result <- server.semanticdbTextDocuments(List(main, api))
    } yield {
      assertEquals(result.schemaVersion, "metals-semanticdb-text-documents.v1")
      val documents = result.documents.asScala
      assertEquals(documents.map(_.uri).sorted, documents.map(_.uri))
      assertEquals(documents.size, 2)
      assertEquals(
        documents.map(_.uri).toSet,
        Set(server.toPath(api).toURI.toString, server.toPath(main).toURI.toString),
      )
      val definitionSymbols = documents.flatMap(_.occurrences.asScala).collect {
        case occurrence if occurrence.role == "DEFINITION" => occurrence.symbol
      }.toSet
      val referenceSymbols = documents.flatMap(_.occurrences.asScala).collect {
        case occurrence if occurrence.role == "REFERENCE" => occurrence.symbol
      }.toSet
      assert(
        definitionSymbols.intersect(referenceSymbols).exists(_.contains("answer")),
        clues(definitionSymbols, referenceSymbols),
      )
      assert(
        documents.flatMap(_.symbols.asScala).exists(_.displayName == "answer")
      )
    }
  }
}
