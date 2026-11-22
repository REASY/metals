package tests

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.jdk.CollectionConverters._

import scala.meta.internal.metals.ScalaCallableEvidence
import scala.meta.internal.metals.SemanticdbTextDocumentsParams
import scala.meta.internal.metals.SemanticdbTextDocumentsResult
import scala.meta.internal.{semanticdb => s}

import com.google.gson.Gson

class ScalaCallableEvidenceSuite extends BaseSuite {
  private val callable = "model/pool/Waiting.apply()."
  private val formal = callable + "(ongoingRequest)"
  private val range = s.Range(1, 10, 1, 17)
  private val parameter = s.SymbolInformation(
    symbol = formal,
    displayName = "ongoingRequest",
    kind = s.SymbolInformation.Kind.PARAMETER,
    signature =
      s.ValueSignature(s.TypeRef(s.NoType, "model/Flow.RequestContext#")),
  )
  private val method = s.SymbolInformation(
    symbol = callable,
    displayName = "apply",
    kind = s.SymbolInformation.Kind.METHOD,
    signature = s.MethodSignature(
      parameterLists = Seq(s.Scope(symlinks = Seq(formal))),
      returnType = s.TypeRef(s.NoType, "model/pool/Waiting#"),
    ),
  )
  private val document = s.TextDocument(
    uri = "file:///workspace/Model.scala",
    md5 = "e2d84a55f2b1df45385ac9d0397bf849",
    language = s.Language.SCALA,
    symbols = Seq(method, parameter),
    synthetics = Seq(
      s.Synthetic(
        Some(range),
        s.SelectTree(s.OriginalTree(Some(range)), Some(s.IdTree(callable))),
      )
    ),
  )
  private def project(doc: s.TextDocument) =
    ScalaCallableEvidence
      .fromSemanticdb(s.TextDocuments(Seq(doc)))
      .documents
      .get(0)

  test("qualified-parameter-and-synthetic-selection-survive-projection") {
    val result = project(document)
    assertEquals(result.status, "AVAILABLE")
    assertEquals(result.selections.get(0).symbol, callable)
    val signature = result.callables.get(0)
    assertEquals(signature.status, "AVAILABLE")
    assertEquals(
      signature.parameterLists.get(0).get(0).tpe.get("symbol").getAsString,
      "model/Flow.RequestContext#",
    )
    assertEquals(
      SemanticdbTextDocumentsResult.SchemaVersion,
      "metals-semanticdb-text-documents.v2",
    )
    assertEquals(
      ScalaCallableEvidence.SchemaVersion,
      "metals-scala-callable-evidence.v2",
    )
  }

  test("authored-overloads-have-definitions-generated-selection-does-not") {
    val authored = Seq(
      method.withSymbol("model/pool/Waiting.apply(+1)."),
      method.withSymbol("model/pool/Waiting.apply(+2)."),
    )
    val definitions = authored.zipWithIndex.map { case (info, index) =>
      s.SymbolOccurrence(
        range = Some(s.Range(index + 3, 6, index + 3, 11)),
        symbol = info.symbol,
        role = s.SymbolOccurrence.Role.DEFINITION,
      )
    }
    val projected = project(
      document
        .withSymbols(Seq(method, parameter) ++ authored)
        .withOccurrences(definitions)
    )
    assertEquals(projected.status, "AVAILABLE")
    assertEquals(projected.selections.get(0).symbol, callable)
    assertEquals(
      projected.definitions.asScala.map(_.symbol).toSet,
      authored.map(_.symbol).toSet,
    )
    assert(!projected.definitions.asScala.exists(_.symbol == callable))
  }

  test("quoted-owner-keeps-selected-overload-and-complete-signature") {
    val selected = "model/pool/`Good-Entry`.apply(+1)."
    val selectedFormal = selected + "(ongoingRequest)"
    val quotedMethod = method
      .withSymbol(selected)
      .withSignature(
        s.MethodSignature(
          parameterLists = Seq(s.Scope(symlinks = Seq(selectedFormal))),
          returnType = s.TypeRef(s.NoType, "model/pool/`Good-Entry`#"),
        )
      )
    val projected = project(
      document
        .withSymbols(
          Seq(quotedMethod, parameter.withSymbol(selectedFormal))
        )
        .withSynthetics(
          Seq(
            s.Synthetic(
              Some(range),
              s.SelectTree(
                s.OriginalTree(Some(range)),
                Some(s.IdTree(selected)),
              ),
            )
          )
        )
    )
    assertEquals(projected.status, "AVAILABLE")
    assertEquals(projected.selections.get(0).symbol, selected)
    val signature = projected.callables.get(0)
    assertEquals(signature.symbol, selected)
    assertEquals(signature.status, "AVAILABLE")
    assertEquals(signature.parameterLists.get(0).get(0).symbol, selectedFormal)
    assertEquals(
      signature.parameterLists.get(0).get(0).tpe.get("symbol").getAsString,
      "model/Flow.RequestContext#",
    )
    assertEquals(
      signature.returnType.get("symbol").getAsString,
      "model/pool/`Good-Entry`#",
    )
  }

  test("mismatched-original-range-does-not-select-a-callable") {
    val result = project(
      document.withSynthetics(
        Seq(
          s.Synthetic(
            Some(range),
            s.SelectTree(
              s.OriginalTree(Some(s.Range(2, 10, 2, 17))),
              Some(s.IdTree(callable)),
            ),
          )
        )
      )
    )
    assert(result.selections.isEmpty)
    assertEquals(result.unsupportedSyntheticCount, 1)
  }

  test("missing-formal-and-unsupported-type-never-expose-partial-signature") {
    for (
      doc <- Seq(
        document.withSymbols(Seq(method)),
        document.withSymbols(
          Seq(method, parameter.withSignature(s.ValueSignature(s.NoType)))
        ),
      )
    ) {
      val result = project(doc).callables.get(0)
      assertEquals(result.status, "UNSUPPORTED_SIGNATURE")
      assert(result.parameterLists.isEmpty)
      assertEquals(result.returnType, null)
    }
  }

  test(
    "compiler-errors-unavailable-sources-and-duplicate-symbols-are-explicit"
  ) {
    for (
      (doc, reason) <- Seq(
        document.withDiagnostics(
          Seq(s.Diagnostic(severity = s.Diagnostic.Severity.ERROR))
        ) -> "COMPILER_ERROR",
        document.withMd5("") -> "SOURCE_UNAVAILABLE",
        document
          .withSymbols(Seq(method, method, parameter)) -> "DUPLICATE_SYMBOL",
        document.withSynthetics(
          Seq.fill(ScalaCallableEvidence.MaxSynthetics + 1)(
            document.synthetics.head
          )
        ) -> "DOCUMENT_FACT_BOUND",
      )
    ) {
      val result = project(doc)
      assertEquals(result.status, "UNAVAILABLE")
      assertEquals(result.reason, reason)
      assert(result.selections.isEmpty && result.callables.isEmpty)
    }
  }

  test("request-bound-and-uri-identity-are-validated") {
    def valid(uris: Seq[String]) =
      ScalaCallableEvidence.valid(SemanticdbTextDocumentsParams(uris.asJava))
    assert(valid(Seq("file:///workspace/Model.scala")))
    assert(!valid(Seq.empty))
    assert(!valid(Seq.fill(33)("file:///workspace/Model.scala")))
    assert(!valid(Seq.fill(2)("file:///workspace/Model.scala")))
    assert(!valid(Seq("https://example.com/Model.scala")))
    assert(!valid(Seq("file:///workspace/Model.scala?other=target")))
    assert(!valid(Seq("file://remote/Model.scala")))
  }

  sys.props
    .get("codehop.rawSemanticdbInput")
    .orElse(sys.env.get("CODEHOP_RAW_SEMANTICDB_INPUT"))
    .foreach { input =>
      test(
        "raw-public-compiler-fixture-retains-generated-and-overloaded-selection"
      ) {
        val doc = s.TextDocument.parseFrom(Files.readAllBytes(Paths.get(input)))
        val result =
          ScalaCallableEvidence.fromSemanticdb(s.TextDocuments(Seq(doc)))
        val projected = result.documents.get(0)
        assertEquals(projected.status, "AVAILABLE")
        val selections = projected.selections.asScala.map(_.symbol).toSet
        assert(selections.contains(callable))
        assert(selections.contains("model/pool/Overloaded.apply(+1)."))
        val selected =
          projected.callables.asScala.find(_.symbol == callable).get
        assertEquals(selected.status, "AVAILABLE")
        assertEquals(
          selected.parameterLists.get(0).get(0).tpe.get("symbol").getAsString,
          "model/Flow.RequestContext#",
        )
        val output = Paths.get(input).resolveSibling("callable-evidence.json")
        Files.write(
          output,
          new Gson().toJson(result).getBytes(StandardCharsets.UTF_8),
        )
        ()
      }
    }
}

/** Offline projection of one retained compiler document bundle; no LSP session. */
object ScalaCallableEvidenceDump {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "expected input protobuf and output JSON paths")
    val input =
      s.TextDocuments.parseFrom(Files.readAllBytes(Paths.get(args(0))))
    val projected = ScalaCallableEvidence.fromSemanticdb(input)
    Files.write(
      Paths.get(args(1)),
      new Gson().toJson(projected).getBytes(StandardCharsets.UTF_8),
    )
    ()
  }
}
