package scala.meta.internal.metals

import java.{util => ju}

import scala.meta.internal.{semanticdb => s}

import _root_.scala.jdk.CollectionConverters._
import org.eclipse.{lsp4j => l}

case class SemanticdbTextDocumentsParams(uris: ju.List[String])

case class SemanticdbTextDocumentsResult(
    schemaVersion: String,
    documents: ju.List[SemanticdbTextDocument],
)

case class SemanticdbTextDocument(
    uri: String,
    md5: String,
    language: String,
    generationKind: String,
    diagnostics: ju.List[SemanticdbDiagnostic],
    occurrences: ju.List[SemanticdbSymbolOccurrence],
    symbols: ju.List[SemanticdbSymbolInformation],
)

case class SemanticdbDiagnostic(
    range: l.Range,
    severity: String,
)

case class SemanticdbSymbolOccurrence(
    range: l.Range,
    symbol: String,
    role: String,
)

case class SemanticdbSymbolInformation(
    symbol: String,
    language: String,
    kind: String,
    properties: Int,
    displayName: String,
    overriddenSymbols: ju.List[String],
)

object SemanticdbTextDocumentsResult {
  val SchemaVersion = "metals-semanticdb-text-documents.v2"
  val BuildTargetPresentationCompilerGenerationKind =
    "BUILD_TARGET_PRESENTATION_COMPILER"
  val UnavailableGenerationKind = "UNAVAILABLE"

  def fromSemanticdb(
      documents: s.TextDocuments
  ): SemanticdbTextDocumentsResult =
    SemanticdbTextDocumentsResult(
      SchemaVersion,
      documents.documents.sortBy(_.uri).map(toDocument).asJava,
    )

  private def toDocument(document: s.TextDocument): SemanticdbTextDocument =
    SemanticdbTextDocument(
      document.uri,
      document.md5,
      document.language.name,
      if (isUnavailablePlaceholder(document)) UnavailableGenerationKind
      else BuildTargetPresentationCompilerGenerationKind,
      document.diagnostics.map(toDiagnostic).asJava,
      document.occurrences.map(toOccurrence).asJava,
      document.symbols.map(toSymbolInformation).asJava,
    )

  private def isUnavailablePlaceholder(document: s.TextDocument): Boolean =
    // Per-file compiler failures retain only the requested URI. An empty source
    // still has a source digest and must not be confused with this placeholder.
    document == s.TextDocument.defaultInstance.withUri(document.uri)

  private def toDiagnostic(diagnostic: s.Diagnostic): SemanticdbDiagnostic =
    SemanticdbDiagnostic(
      diagnostic.range.map(toRange).orNull,
      diagnostic.severity.name,
    )

  private def toOccurrence(
      occurrence: s.SymbolOccurrence
  ): SemanticdbSymbolOccurrence =
    SemanticdbSymbolOccurrence(
      occurrence.range.map(toRange).orNull,
      occurrence.symbol,
      occurrence.role.name,
    )

  private def toSymbolInformation(
      information: s.SymbolInformation
  ): SemanticdbSymbolInformation =
    SemanticdbSymbolInformation(
      information.symbol,
      information.language.name,
      information.kind.name,
      information.properties,
      information.displayName,
      information.overriddenSymbols.asJava,
    )

  private def toRange(range: s.Range): l.Range =
    new l.Range(
      new l.Position(range.startLine, range.startCharacter),
      new l.Position(range.endLine, range.endCharacter),
    )
}
