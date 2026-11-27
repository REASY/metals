package scala.meta.internal.metals

import java.util as ju

import scala.collection.mutable
import scala.meta.internal.semanticdb as s

import _root_.scala.jdk.CollectionConverters.*
import org.eclipse.lsp4j as l

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
    synthetics: ju.List[SemanticdbSynthetic],
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

case class SemanticdbSynthetic(
    range: l.Range,
    selectedSymbol: String,
    hasImplicitConversion: Boolean,
    hasImplicitArguments: Boolean,
    hasMacroExpansion: Boolean,
    symbols: ju.List[String],
)

object SemanticdbTextDocumentsResult {
  val SchemaVersion = "metals-semanticdb-text-documents.v3"
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
      document.synthetics.flatMap(toSynthetic).sortBy(syntheticSortKey).asJava,
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

  private def toSynthetic(
      synthetic: s.Synthetic
  ): Option[SemanticdbSynthetic] =
    synthetic.range.flatMap { range =>
      val selectedSymbol = extractSelectedSymbol(synthetic.tree, range)
      var hasImplicitConversion = false
      var hasImplicitArguments = false
      var hasMacroExpansion = false
      val symbols = mutable.LinkedHashSet.empty[String]

      def visit(tree: s.Tree): Unit = tree match {
        case s.IdTree(symbol) =>
          if (symbol.nonEmpty) symbols += symbol
        case s.ApplyTree(function, arguments, _) =>
          if (arguments.exists(containsOriginalTree)) {
            hasImplicitConversion = true
          }
          if (containsOriginalTree(function) && arguments.nonEmpty) {
            hasImplicitArguments = true
          }
          visit(function)
          arguments.foreach(visit)
        case s.SelectTree(qualifier, id) =>
          visit(qualifier)
          id.foreach(visit)
        case s.TypeApplyTree(function, _) =>
          visit(function)
        case s.FunctionTree(parameters, body) =>
          parameters.foreach(visit)
          visit(body)
        case s.MacroExpansionTree(beforeExpansion, _) =>
          hasMacroExpansion = true
          visit(beforeExpansion)
        case _ => ()
      }

      visit(synthetic.tree)
      if (
        selectedSymbol.isEmpty &&
        !hasImplicitConversion &&
        !hasImplicitArguments &&
        !hasMacroExpansion &&
        symbols.isEmpty
      ) {
        None
      } else {
        Some(
          SemanticdbSynthetic(
            toRange(range),
            selectedSymbol,
            hasImplicitConversion,
            hasImplicitArguments,
            hasMacroExpansion,
            symbols.toList.asJava,
          )
        )
      }
    }

  private def extractSelectedSymbol(
      tree: s.Tree,
      expectedRange: s.Range,
  ): String = tree match {
    case s.SelectTree(
          s.OriginalTree(original),
          Some(s.IdTree(symbol)),
        ) if original.forall(_ == expectedRange) && symbol.nonEmpty =>
      symbol
    case s.TypeApplyTree(function, _) =>
      extractSelectedSymbol(function, expectedRange)
    case s.ApplyTree(function, _, _) =>
      extractSelectedSymbol(function, expectedRange)
    case _ => ""
  }

  private def containsOriginalTree(tree: s.Tree): Boolean = tree match {
    case _: s.OriginalTree => true
    case s.ApplyTree(function, arguments, _) =>
      containsOriginalTree(function) || arguments.exists(containsOriginalTree)
    case s.SelectTree(qualifier, _) => containsOriginalTree(qualifier)
    case s.TypeApplyTree(function, _) => containsOriginalTree(function)
    case s.FunctionTree(_, body) => containsOriginalTree(body)
    case s.MacroExpansionTree(beforeExpansion, _) =>
      containsOriginalTree(beforeExpansion)
    case _ => false
  }

  private def syntheticSortKey(
      synthetic: SemanticdbSynthetic
  ): (Int, Int, Int, Int, String, Boolean, Boolean, Boolean, String) = (
    synthetic.range.getStart.getLine,
    synthetic.range.getStart.getCharacter,
    synthetic.range.getEnd.getLine,
    synthetic.range.getEnd.getCharacter,
    synthetic.selectedSymbol,
    synthetic.hasImplicitConversion,
    synthetic.hasImplicitArguments,
    synthetic.hasMacroExpansion,
    synthetic.symbols.asScala.mkString(","),
  )
}
