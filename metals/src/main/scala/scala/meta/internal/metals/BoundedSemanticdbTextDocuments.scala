package scala.meta.internal.metals

import java.io.OutputStream
import java.io.OutputStreamWriter
import java.nio.charset.StandardCharsets
import java.util.Collections
import java.{util => ju}

import scala.jdk.CollectionConverters._
import scala.util.control.NoStackTrace

import scala.meta.pc.CancelToken

import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode

case class BoundedSemanticdbTextDocumentsParams(
    uris: ju.List[String],
    maxResponseBytes: Int,
)

/** The byte budget covers the UTF-8 JSON result, excluding the RPC envelope. */
case class BoundedSemanticdbTextDocumentsResult(
    format: String,
    schemaVersion: String,
    maxResponseBytes: Int,
    documents: ju.List[SemanticdbTextDocument],
    deferredUris: ju.List[String],
    oversizedUris: ju.List[String],
    unavailableUris: ju.List[String],
)

object BoundedSemanticdbTextDocuments {
  val Format = "codehop.metals-semanticdb-bounded.v1"
  val MaxResponseBytes: Int = 8 * 1024 * 1024
  private val gson = new MessageJsonHandler(Collections.emptyMap()).getGson
  private case object ByteBudgetExceeded
      extends RuntimeException
      with NoStackTrace

  private def invalid(): Nothing =
    throw new ResponseErrorException(
      new ResponseError(
        ResponseErrorCode.InvalidParams,
        "Invalid bounded SemanticDB request or insufficient metadata budget",
        null,
      )
    )

  def validate(params: BoundedSemanticdbTextDocumentsParams): Unit = {
    if (
      params == null || params.maxResponseBytes < 1024 ||
      params.maxResponseBytes > MaxResponseBytes ||
      !ScalaCallableEvidence.valid(SemanticdbTextDocumentsParams(params.uris))
    ) invalid()
    // Prove that even a wholly deferred response can report every requested URI.
    if (encodedSize(empty(params), params.maxResponseBytes).isEmpty) invalid()
  }

  private def empty(
      params: BoundedSemanticdbTextDocumentsParams
  ): BoundedSemanticdbTextDocumentsResult =
    BoundedSemanticdbTextDocumentsResult(
      Format,
      SemanticdbTextDocumentsResult.SchemaVersion,
      params.maxResponseBytes,
      ju.Collections.emptyList(),
      params.uris.asScala.sorted.asJava,
      ju.Collections.emptyList(),
      ju.Collections.emptyList(),
    )

  /**
   * Counting through the actual LSP Gson/UTF-8 encoder also bounds temporary
   * serialization memory for a single pathological compiler document.
   */
  private def encodedSize(
      value: AnyRef,
      limit: Long,
      token: CancelToken = EmptyCancelToken,
  ): Option[Long] = {
    token.checkCanceled()
    var bytes = 0L
    val sink = new OutputStream {
      override def write(value: Int): Unit = add(1)
      override def write(value: Array[Byte], offset: Int, length: Int): Unit =
        add(length)
      private def add(length: Int): Unit = {
        token.checkCanceled()
        bytes += length
        if (bytes > limit) throw ByteBudgetExceeded
      }
    }
    val writer = new OutputStreamWriter(sink, StandardCharsets.UTF_8)
    try {
      gson.toJson(value, writer)
      writer.flush()
      Some(bytes)
    } catch {
      case ByteBudgetExceeded => None
    }
  }

  def bounded(
      params: BoundedSemanticdbTextDocumentsParams,
      acquired: SemanticdbTextDocumentsResult,
      token: CancelToken = EmptyCancelToken,
  ): BoundedSemanticdbTextDocumentsResult = {
    token.checkCanceled()
    validate(params)
    val requested = params.uris.asScala.toSet
    val documents = acquired.documents.asScala.sortBy(_.uri)
    if (
      acquired.schemaVersion != SemanticdbTextDocumentsResult.SchemaVersion ||
      documents.exists(doc => !requested(doc.uri)) ||
      documents.map(_.uri).distinct.size != documents.size
    ) invalid()
    var result = empty(params)
    for (document <- documents) {
      token.checkCanceled()
      val remaining = result.deferredUris.asScala.filterNot(_ == document.uri)
      val candidate = result.copy(
        documents = (result.documents.asScala :+ document).asJava,
        deferredUris = remaining.asJava,
      )
      if (encodedSize(candidate, params.maxResponseBytes, token).isDefined)
        result = candidate
      else if (params.uris.size == 1) {
        result = result.copy(
          deferredUris = remaining.asJava,
          oversizedUris = List(document.uri).asJava,
        )
      }
    }
    val missing = requested -- documents.map(_.uri)
    result = result.copy(
      deferredUris = result.deferredUris.asScala.filterNot(missing).asJava,
      unavailableUris = missing.toSeq.sorted.asJava,
    )
    // Moving URIs between terminal arrays cannot silently exceed the budget.
    if (encodedSize(result, params.maxResponseBytes, token).isEmpty) invalid()
    result
  }
}
