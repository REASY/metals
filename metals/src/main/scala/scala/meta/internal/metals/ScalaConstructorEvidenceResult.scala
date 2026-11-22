package scala.meta.internal.metals

import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.meta.pc.ScalaConstructorEvidence

import com.google.gson.Gson

case class ScalaConstructorEvidenceResult(
    schemaVersion: String,
    status: String,
    reason: String,
    documents: ju.List[ScalaConstructorEvidence],
)

object ScalaConstructorEvidenceResult {
  val MaxDocuments = 16
  val MaxResponseBytes: Int = 8 * 1024 * 1024

  def unavailable(reason: String): ScalaConstructorEvidenceResult =
    ScalaConstructorEvidenceResult(
      ScalaConstructorEvidence.SCHEMA_VERSION,
      "UNAVAILABLE",
      reason,
      ju.Collections.emptyList(),
    )

  def bounded(
      result: ScalaConstructorEvidenceResult
  ): ScalaConstructorEvidenceResult =
    if (
      new Gson()
        .toJson(result)
        .getBytes(StandardCharsets.UTF_8)
        .length > MaxResponseBytes
    )
      unavailable("RESPONSE_BOUND")
    else result

  def valid(params: SemanticdbTextDocumentsParams): Boolean =
    ScalaCallableEvidence.valid(params) && params.uris.size <= MaxDocuments
}
