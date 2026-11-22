package scala.meta.internal.metals

import java.net.URI
import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.jdk.CollectionConverters._
import scala.util.Try

import scala.meta.internal.{semanticdb => s}

import com.google.gson.Gson
import com.google.gson.JsonArray
import com.google.gson.JsonObject
import org.eclipse.{lsp4j => l}

case class ScalaCallableEvidenceResult(
    schemaVersion: String,
    status: String,
    reason: String,
    documents: ju.List[ScalaCallableEvidenceDocument],
)
case class ScalaCallableEvidenceDocument(
    uri: String,
    md5: String,
    status: String,
    reason: String,
    selections: ju.List[ScalaCallableSelection],
    callables: ju.List[ScalaCallableSignature],
    definitions: ju.List[ScalaCallableDefinition],
    unsupportedSyntheticCount: Int,
)
case class ScalaCallableSelection(range: l.Range, symbol: String)
case class ScalaCallableDefinition(range: l.Range, symbol: String)
case class ScalaCallableSignature(
    symbol: String,
    displayName: String,
    kind: String,
    properties: Int,
    status: String,
    parameterLists: ju.List[ju.List[ScalaCallableParameter]],
    returnType: JsonObject,
)
case class ScalaCallableParameter(
    symbol: String,
    displayName: String,
    properties: Int,
    tpe: JsonObject,
)

/** A bounded projection of compiler facts, never a source-name resolver. */
object ScalaCallableEvidence {
  val SchemaVersion = "metals-scala-callable-evidence.v2"
  val MaxDocuments = 32
  val MaxSymbols = 20000
  val MaxSynthetics = 4096
  val MaxDefinitions = 4096
  val MaxResponseBytes: Int = 8 * 1024 * 1024

  def valid(params: SemanticdbTextDocumentsParams): Boolean =
    params != null && params.uris != null && !params.uris.isEmpty &&
      params.uris.size <= MaxDocuments &&
      params.uris.asScala.forall(uri =>
        uri != null && uri.length <= 8192 && Try {
          val parsed = new URI(uri)
          parsed.getScheme == "file" && parsed.getRawQuery == null &&
          parsed.getRawFragment == null && parsed.getPath.startsWith("/") &&
          (parsed.getAuthority == null || parsed.getAuthority == "")
        }.getOrElse(false)
      ) && params.uris.asScala.distinct.size == params.uris.size

  def unavailable(reason: String): ScalaCallableEvidenceResult =
    ScalaCallableEvidenceResult(
      SchemaVersion,
      "UNAVAILABLE",
      reason,
      ju.Collections.emptyList(),
    )

  def fromSemanticdb(documents: s.TextDocuments): ScalaCallableEvidenceResult =
    if (documents.documents.size > MaxDocuments)
      unavailable("DOCUMENT_BOUND")
    else
      bounded(
        ScalaCallableEvidenceResult(
          SchemaVersion,
          "AVAILABLE",
          "",
          documents.documents.sortBy(_.uri).map(projectDocument).asJava,
        )
      )

  def bounded(
      result: ScalaCallableEvidenceResult
  ): ScalaCallableEvidenceResult =
    if (
      new Gson()
        .toJson(result)
        .getBytes(StandardCharsets.UTF_8)
        .length > MaxResponseBytes
    )
      unavailable("RESPONSE_BOUND")
    else result

  private def projectDocument(
      doc: s.TextDocument
  ): ScalaCallableEvidenceDocument = {
    val reason =
      if (doc.language != s.Language.SCALA || doc.md5.isEmpty)
        "SOURCE_UNAVAILABLE"
      else if (
        doc.diagnostics.exists(_.severity == s.Diagnostic.Severity.ERROR)
      )
        "COMPILER_ERROR"
      else if (
        doc.symbols.size > MaxSymbols || doc.synthetics.size > MaxSynthetics
      )
        "DOCUMENT_FACT_BOUND"
      else if (doc.symbols.map(_.symbol).distinct.size != doc.symbols.size)
        "DUPLICATE_SYMBOL"
      else ""
    if (reason.nonEmpty)
      ScalaCallableEvidenceDocument(
        doc.uri,
        doc.md5,
        "UNAVAILABLE",
        reason,
        ju.Collections.emptyList(),
        ju.Collections.emptyList(),
        ju.Collections.emptyList(),
        0,
      )
    else {
      val symbols = doc.symbols.map(info => info.symbol -> info).toMap
      val selections = doc.synthetics.collect {
        case s.Synthetic(
              Some(range),
              s.SelectTree(
                s.OriginalTree(Some(original)),
                Some(s.IdTree(symbol)),
              ),
            ) if range == original && symbol.nonEmpty =>
          ScalaCallableSelection(toRange(range), symbol)
      }
      val callables = doc.symbols
        .collect {
          case info if info.signature.isInstanceOf[s.MethodSignature] =>
            projectCallable(info, symbols)
        }
        .sortBy(_.symbol)
      // Definitions are source declarations, not inferred callable origins.
      // A generated method normally has no definition occurrence; consumers
      // must prove a complete physical authored catalog before using absence.
      val definitions = doc.occurrences
        .collect {
          case s.SymbolOccurrence(
                Some(range),
                symbol,
                s.SymbolOccurrence.Role.DEFINITION,
              )
              if symbols
                .get(symbol)
                .exists(info =>
                  info.displayName == "apply" &&
                    info.signature.isInstanceOf[s.MethodSignature]
                ) =>
            ScalaCallableDefinition(toRange(range), symbol)
        }
        .sortBy(definition =>
          (
            definition.symbol,
            definition.range.getStart.getLine,
            definition.range.getStart.getCharacter,
          )
        )
      if (definitions.size > MaxDefinitions)
        ScalaCallableEvidenceDocument(
          doc.uri,
          doc.md5,
          "UNAVAILABLE",
          "DEFINITION_BOUND",
          ju.Collections.emptyList(),
          ju.Collections.emptyList(),
          ju.Collections.emptyList(),
          0,
        )
      else
        ScalaCallableEvidenceDocument(
          doc.uri,
          doc.md5,
          "AVAILABLE",
          "",
          selections.asJava,
          callables.asJava,
          definitions.asJava,
          doc.synthetics.size - selections.size,
        )
    }
  }

  private def projectCallable(
      info: s.SymbolInformation,
      symbols: Map[String, s.SymbolInformation],
  ): ScalaCallableSignature = {
    val signature = info.signature.asInstanceOf[s.MethodSignature]
    val budget = new TypeBudget
    var complete = signature.typeParameters.forall(scope =>
      scope.symlinks.isEmpty && scope.hardlinks.isEmpty
    ) && signature.parameterLists.size <= 16
    val lists = signature.parameterLists.take(16).map { scope =>
      val parameters =
        if (scope.hardlinks.nonEmpty || scope.symlinks.size > 128) {
          complete = false
          Seq.empty
        } else
          scope.symlinks.flatMap { symbol =>
            symbols.get(symbol) match {
              case Some(parameter)
                  if parameter.signature.isInstanceOf[s.ValueSignature] =>
                val tpe = projectType(
                  parameter.signature.asInstanceOf[s.ValueSignature].tpe,
                  budget,
                  0,
                )
                if (tpe.isEmpty) complete = false
                Some(
                  ScalaCallableParameter(
                    symbol,
                    parameter.displayName,
                    parameter.properties,
                    tpe.orNull,
                  )
                )
              case _ =>
                complete = false
                None
            }
          }
      parameters.asJava
    }
    val result = projectType(signature.returnType, budget, 0)
    if (result.isEmpty) complete = false
    // Unsupported signatures never expose a truncated positive formal vector.
    ScalaCallableSignature(
      info.symbol,
      info.displayName,
      info.kind.name,
      info.properties,
      if (complete) "AVAILABLE" else "UNSUPPORTED_SIGNATURE",
      if (complete) lists.asJava else ju.Collections.emptyList(),
      if (complete) result.orNull else null,
    )
  }

  private class TypeBudget { var remaining: Int = 256 }

  private def projectType(
      tpe: s.Type,
      budget: TypeBudget,
      depth: Int,
  ): Option[JsonObject] = {
    budget.remaining -= 1
    if (depth > 16 || budget.remaining < 0) None
    else {
      val result = new JsonObject
      def reference(
          kind: String,
          prefix: s.Type,
          symbol: String,
          arguments: Seq[s.Type],
      ): Option[JsonObject] = {
        val projectedPrefix =
          if (prefix == s.NoType) Some(None)
          else projectType(prefix, budget, depth + 1).map(Some(_))
        val projectedArguments =
          arguments.take(128).map(projectType(_, budget, depth + 1))
        if (
          symbol.isEmpty || arguments.size > 128 || projectedPrefix.isEmpty || projectedArguments
            .exists(_.isEmpty)
        ) None
        else {
          result.addProperty("kind", kind)
          result.addProperty("symbol", symbol)
          projectedPrefix.flatten.foreach(result.add("prefix", _))
          val array = new JsonArray
          projectedArguments.flatten.foreach(array.add)
          result.add("arguments", array)
          Some(result)
        }
      }
      def wrapped(kind: String, underlying: s.Type): Option[JsonObject] =
        projectType(underlying, budget, depth + 1).map { value =>
          result.addProperty("kind", kind)
          result.add("underlying", value)
          result
        }
      tpe match {
        case s.TypeRef(prefix, symbol, arguments) =>
          reference("type_ref", prefix, symbol, arguments)
        case s.SingleType(prefix, symbol) =>
          reference("single_type", prefix, symbol, Seq.empty)
        case s.ThisType(symbol) =>
          reference("this_type", s.NoType, symbol, Seq.empty)
        case s.ByNameType(underlying) => wrapped("by_name", underlying)
        case s.RepeatedType(underlying) => wrapped("repeated", underlying)
        case _ => None
      }
    }
  }

  private def toRange(range: s.Range): l.Range =
    new l.Range(
      new l.Position(range.startLine, range.startCharacter),
      new l.Position(range.endLine, range.endCharacter),
    )
}
