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
    occurrences: ju.List[SemanticdbSymbolOccurrence],
    symbols: ju.List[SemanticdbSymbolInformation],
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
  val SchemaVersion = "metals-semanticdb-text-documents.v1"

  def fromSemanticdb(documents: s.TextDocuments): SemanticdbTextDocumentsResult =
    SemanticdbTextDocumentsResult(
      SchemaVersion,
      documents.documents.sortBy(_.uri).map(toDocument).asJava,
    )

  private def toDocument(document: s.TextDocument): SemanticdbTextDocument =
    SemanticdbTextDocument(
      document.uri,
      document.md5,
      document.language.name,
      document.occurrences.map(toOccurrence).asJava,
      document.symbols.map(toSymbolInformation).asJava,
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
