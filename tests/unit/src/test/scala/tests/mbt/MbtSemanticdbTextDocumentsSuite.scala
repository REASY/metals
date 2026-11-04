package tests.mbt

import scala.jdk.CollectionConverters._

import scala.meta.internal.metals.SemanticdbTextDocumentsResult
import scala.meta.internal.{semanticdb => s}

class MbtSemanticdbTextDocumentsSuite
    extends BaseMbtReferenceSuite("mbt-semanticdb-text-documents") {

  testLSP("batch-generic-map-overrides-retain-exact-overloaded-contract") {
    cleanWorkspace()
    val api = "a/src/main/scala/fixture/Codec.scala"
    val implementation = "a/src/main/scala/fixture/Cell.scala"
    val caller = "a/src/main/scala/fixture/Caller.scala"
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a":{"scalaVersion":"2.13.18"}}
            |/$api
            |package fixture
            |trait Codec[A] {
            |  def decode(data: Map[String, String]): A
            |  def decode(data: String): A
            |}
            |/$implementation
            |package fixture
            |final class Cell[A](default: A) extends Codec[A] {
            |  override def decode(data: Map[String, String]): A = default
            |  override def decode(data: String): A = default
            |}
            |final class Unrelated[A](default: A) {
            |  def decode(data: Map[String, String]): A = default
            |}
            |/$caller
            |package fixture
            |object Caller {
            |  def run[A](codec: Codec[A], data: Map[String, String]): A = codec.decode(data)
            |}
            |""".stripMargin
      )
      _ <- server.didOpenAndFocus(caller)
      result <- server.semanticdbTextDocuments(
        List(caller, api, implementation)
      )
    } yield {
      val documents = result.documents.asScala
      assertEquals(documents.size, 3)
      assert(
        documents.forall(
          _.generationKind == "BUILD_TARGET_PRESENTATION_COMPILER"
        )
      )
      assert(documents.forall(_.diagnostics.isEmpty))
      val symbols = documents.flatMap(_.symbols.asScala)
      val implementations = symbols.filter(info =>
        info.kind == "METHOD" && info.symbol.startsWith("fixture/Cell#decode(")
      )
      assertEquals(implementations.size, 2)
      val selected = documents
        .find(_.uri.endsWith("/Caller.scala"))
        .get
        .occurrences
        .asScala
        .filter(occurrence =>
          occurrence.role == "REFERENCE" && occurrence.symbol.startsWith(
            "fixture/Codec#decode("
          )
        )
        .map(_.symbol)
        .toSet
      assertEquals(selected.size, 1)
      val overrides = implementations.map(_.overriddenSymbols.asScala.toSet)
      assert(overrides.forall(_.size == 1), clues(overrides))
      assertEquals(overrides.flatten.toSet.size, 2)
      assertEquals(overrides.count(_ == selected), 1)
      assert(
        symbols
          .filter(_.symbol.startsWith("fixture/Unrelated#decode("))
          .forall(_.overriddenSymbols.isEmpty)
      )
      println(
        s"generic-map-override-oracle: selected=$selected overrides=$overrides unrelated=none"
      )
    }
  }

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
      assertEquals(result.schemaVersion, "metals-semanticdb-text-documents.v2")
      val documents = result.documents.asScala
      assert(
        documents.forall(
          _.generationKind == "BUILD_TARGET_PRESENTATION_COMPILER"
        )
      )
      assertEquals(documents.map(_.uri).sorted, documents.map(_.uri))
      assertEquals(documents.size, 2)
      assertEquals(
        documents.map(_.uri).toSet,
        Set(
          server.toPath(api).toURI.toString,
          server.toPath(main).toURI.toString,
        ),
      )
      val definitionSymbols = documents
        .flatMap(_.occurrences.asScala)
        .collect {
          case occurrence if occurrence.role == "DEFINITION" =>
            occurrence.symbol
        }
        .toSet
      val referenceSymbols = documents
        .flatMap(_.occurrences.asScala)
        .collect {
          case occurrence if occurrence.role == "REFERENCE" => occurrence.symbol
        }
        .toSet
      assert(
        definitionSymbols
          .intersect(referenceSymbols)
          .exists(_.contains("answer")),
        clues(definitionSymbols, referenceSymbols),
      )
      assert(
        documents.flatMap(_.symbols.asScala).exists(_.displayName == "answer")
      )
    }
  }

  testLSP("batch-java-overloaded-static-import") {
    cleanWorkspace()
    val main = "root/src/main/java/example/Main.java"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "root": {
           |    "libraryDependencies": [
           |      "org.assertj:assertj-core:3.26.3"
           |    ]
           |  }
           |}
           |/$main
           |package example;
           |
           |import static org.assertj.core.api.Assertions.assertThat;
           |
           |class Main {
           |  void check(String expected, String actual) {
           |    assertThat(actual).isEqualTo(expected);
           |  }
           |}
           |""".stripMargin
      )
      _ <- server.didOpenAndFocus(main)
      result <- server.semanticdbTextDocuments(List(main))
    } yield {
      assertEquals(result.schemaVersion, "metals-semanticdb-text-documents.v2")
      assert(
        result.documents.asScala.forall(
          _.generationKind == "BUILD_TARGET_PRESENTATION_COMPILER"
        )
      )
      assert(result.documents.asScala.forall(_.diagnostics.isEmpty))
      val references = result.documents.asScala
        .flatMap(_.occurrences.asScala)
        .collect {
          case occurrence if occurrence.role == "REFERENCE" =>
            occurrence.symbol
        }
      assert(
        references.exists(
          _.startsWith("org/assertj/core/api/Assertions#assertThat(")
        ),
        clues(references),
      )
      assert(
        references.exists(
          _.startsWith("org/assertj/core/api/AbstractStringAssert#isEqualTo(")
        ),
        clues(references),
      )
    }
  }

  testLSP("batch-unowned-file-is-omitted") {
    cleanWorkspace()
    val unowned = "Unowned.java"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "root": {}
           |}
           |/$unowned
           |class Unowned {}
           |""".stripMargin
      )
      result <- server.semanticdbTextDocuments(List(unowned))
    } yield {
      assertEquals(result.schemaVersion, "metals-semanticdb-text-documents.v2")
      assertEquals(result.documents.asScala.toList, Nil)
    }
  }

  testLSP("batch-java-error-diagnostic") {
    cleanWorkspace()
    val main = "root/src/main/java/example/Main.java"
    val healthy = "root/src/main/java/example/Healthy.java"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "root": {}
           |}
           |/$main
           |package example;
           |
           |class Main {
           |  void check() {
           |    MissingApi.call();
           |  }
           |}
           |/$healthy
           |package example;
           |
           |class Healthy {
           |  String trim(String value) {
           |    return value.trim();
           |  }
           |}
           |""".stripMargin
      )
      result <- server.semanticdbTextDocuments(List(main, healthy))
    } yield {
      val documents = result.documents.asScala.toList
      assertEquals(documents.size, 2)
      assertEquals(
        documents.map(_.generationKind).toSet,
        Set("BUILD_TARGET_PRESENTATION_COMPILER"),
      )
      val documentsByUri =
        documents.map(document => document.uri -> document).toMap
      val brokenDocument = documentsByUri(server.toPath(main).toURI.toString)
      val healthyDocument =
        documentsByUri(server.toPath(healthy).toURI.toString)
      assert(
        brokenDocument.diagnostics.asScala.exists(_.severity == "ERROR")
      )
      assertEquals(healthyDocument.diagnostics.asScala.toList, Nil)
      assert(
        healthyDocument.occurrences.asScala.exists(
          _.symbol == "java/lang/String#trim()."
        )
      )
    }
  }

  testLSP("batch-scala-unavailable-beside-healthy-document") {
    cleanWorkspace()
    val healthy = "root/src/main/scala/example/Healthy.scala"
    val broken = "root/src/main/scala/example/Broken.scala"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"root": {}}
           |/$healthy
           |package example
           |object Healthy { def answer(): Int = 42 }
           |/$broken
           |package example
           |object Broken { val result = }
           |""".stripMargin
      )
      result <- server.semanticdbTextDocuments(List(healthy, broken))
    } yield {
      val documents = result.documents.asScala.map(d => d.uri -> d).toMap
      assertEquals(documents.size, 2)
      val unavailable = documents(server.toPath(broken).toURI.toString)
      assertEquals(unavailable.generationKind, "UNAVAILABLE")
      assertEquals(unavailable.md5, "")
      assertEquals(unavailable.language, "UNKNOWN_LANGUAGE")
      assert(unavailable.diagnostics.isEmpty)
      assert(unavailable.occurrences.isEmpty)
      assert(unavailable.symbols.isEmpty)
      val available = documents(server.toPath(healthy).toURI.toString)
      assertEquals(
        available.generationKind,
        "BUILD_TARGET_PRESENTATION_COMPILER",
      )
      assert(available.md5.matches("[a-fA-F0-9]{32}"))
      assert(available.occurrences.asScala.exists(_.symbol.contains("answer")))
    }
  }

  test("failed-compiler-document-is-explicitly-unavailable") {
    val uri = "file:///workspace/src/main/scala/example/Broken.scala"
    val failedDocument = s.TextDocument.defaultInstance.withUri(uri)

    val result = SemanticdbTextDocumentsResult.fromSemanticdb(
      s.TextDocuments(Seq(failedDocument))
    )
    val document = result.documents.asScala.head

    assertEquals(document.uri, uri)
    assertEquals(document.md5, "")
    assertEquals(document.language, "UNKNOWN_LANGUAGE")
    assertEquals(document.generationKind, "UNAVAILABLE")
    assertEquals(document.diagnostics.asScala.toList, Nil)
    assertEquals(document.occurrences.asScala.toList, Nil)
    assertEquals(document.symbols.asScala.toList, Nil)
  }

  test("empty-source-remains-compiler-generated") {
    val source = s.TextDocument.defaultInstance
      .withUri("file:///workspace/Empty.scala")
      .withSchema(s.Schema.SEMANTICDB4)
      .withLanguage(s.Language.SCALA)
      .withMd5("d41d8cd98f00b204e9800998ecf8427e")
    val result = SemanticdbTextDocumentsResult.fromSemanticdb(
      s.TextDocuments(Seq(source))
    )
    val document = result.documents.asScala.head
    assertEquals(document.md5, source.md5)
    assertEquals(document.generationKind, "BUILD_TARGET_PRESENTATION_COMPILER")
  }

  test("partial-documents-are-not-unavailable-placeholders") {
    val empty =
      s.TextDocument.defaultInstance.withUri("file:///workspace/A.scala")
    val partialDocuments = Seq(
      empty.withMd5("invalid"),
      empty.withLanguage(s.Language.SCALA),
      empty.withSchema(s.Schema.SEMANTICDB4),
      empty.withText("object A"),
      empty.withDiagnostics(Seq(s.Diagnostic.defaultInstance)),
      empty.withOccurrences(Seq(s.SymbolOccurrence.defaultInstance)),
      empty.withSymbols(Seq(s.SymbolInformation.defaultInstance)),
      empty.withSynthetics(Seq(s.Synthetic.defaultInstance)),
    )
    partialDocuments.foreach { source =>
      val result = SemanticdbTextDocumentsResult.fromSemanticdb(
        s.TextDocuments(Seq(source))
      )
      assertEquals(
        result.documents.asScala.head.generationKind,
        "BUILD_TARGET_PRESENTATION_COMPILER",
        clues(source),
      )
    }
  }

  test("unavailable-document-preserves-healthy-batch-evidence") {
    val healthy = s.TextDocument.defaultInstance
      .withUri("file:///workspace/A.scala")
      .withLanguage(s.Language.SCALA)
      .withMd5("0123456789abcdef0123456789abcdef")
      .withOccurrences(Seq(s.SymbolOccurrence(symbol = "example/A#")))
    val failed =
      s.TextDocument.defaultInstance.withUri("file:///workspace/Z.scala")
    val result = SemanticdbTextDocumentsResult.fromSemanticdb(
      s.TextDocuments(Seq(failed, healthy))
    )
    val documents = result.documents.asScala
    assertEquals(documents.map(_.uri).toSeq, Seq(healthy.uri, failed.uri))
    assertEquals(
      documents.head.generationKind,
      "BUILD_TARGET_PRESENTATION_COMPILER",
    )
    assertEquals(documents.head.md5, healthy.md5)
    assertEquals(documents.head.occurrences.asScala.head.symbol, "example/A#")
    assertEquals(documents.last.generationKind, "UNAVAILABLE")
  }
}
