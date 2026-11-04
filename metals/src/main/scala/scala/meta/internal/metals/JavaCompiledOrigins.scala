package scala.meta.internal.metals

import java.net.URI
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

import scala.meta.pc.CancelToken

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.google.gson.JsonParser

/** Bounded BSP output ownership for compiler-selected classfile locations. */
object JavaCompiledOrigins {
  val SchemaVersion = "metals-java-compiled-origins.experimental.v2"

  final case class Output(targetId: String, uri: String)
  final case class Snapshot(outputs: Vector[Output], metadata: String)

  def unavailable(reason: String): JsonObject = {
    val result = JavaReceiverTypes.unavailable(reason)
    result.addProperty("schemaVersion", SchemaVersion)
    result
  }

  private def filePath(raw: String): Option[Path] =
    Try(URI.create(raw)).toOption
      .filter(uri =>
        uri.getScheme == "file" && uri.getRawQuery == null &&
          uri.getRawFragment == null &&
          Option(uri.getRawAuthority).forall(_.isEmpty)
      )
      .flatMap(uri => Try(Paths.get(uri).toAbsolutePath.normalize()).toOption)

  def outputOwner(
      origin: String,
      outputs: Vector[Output],
  ): Either[String, Output] =
    outputIndex(outputs).flatMap(indexedOutputOwner(origin, _))

  private def outputIndex(
      outputs: Vector[Output]
  ): Either[String, Vector[(Output, Path)]] = {
    val parsed = outputs.map(output => output -> filePath(output.uri))
    if (parsed.exists(_._2.isEmpty)) Left("INVALID_BSP_OUTPUT_URI")
    else Right(parsed.collect { case (output, Some(path)) => output -> path })
  }

  private def indexedOutputOwner(
      origin: String,
      index: Vector[(Output, Path)],
  ): Either[String, Output] =
    filePath(origin) match {
      case None => Left("INVALID_CLASSFILE_URI")
      case Some(path) =>
        index.collect {
          case (output, root) if path != root && path.startsWith(root) => output
        }.distinct match {
          case Vector(owner) => Right(owner)
          case owners if owners.isEmpty => Left("OUTPUT_OWNER_UNAVAILABLE")
          case _ => Left("AMBIGUOUS_OUTPUT_OWNER")
        }
    }

  def capture(
      targets: BuildTargets,
      requested: BuildTargetIdentifier,
      token: CancelToken,
  ): Either[String, Snapshot] = {
    val visited = mutable.Set.empty[String]
    val outputs = Vector.newBuilder[Output]
    val rows = new JsonArray()
    var pending = List(requested)
    var failure: Option[String] = None
    var outputCount = 0
    while (pending.nonEmpty && failure.isEmpty) {
      token.checkCanceled()
      val current = pending.head
      pending = pending.tail
      if (!visited(current.getUri())) {
        visited += current.getUri()
        if (visited.size > 256 || current.getUri().length > 8192)
          failure = Some("BSP_OUTPUT_METADATA_BUDGET")
        else
          targets.info(current) match {
            case None => failure = Some("BSP_TARGET_UNAVAILABLE")
            case Some(target) =>
              val dependencies =
                target.getDependencies().asScala.toVector.sortBy(_.getUri())
              val directories =
                targets.targetClassDirectories(current).distinct.sorted
              outputCount += directories.size
              if (dependencies.size > 256 || outputCount > 1024)
                failure = Some("BSP_OUTPUT_METADATA_BUDGET")
              else if (directories.isEmpty)
                failure = Some("BSP_OUTPUT_UNAVAILABLE")
              else if (
                directories
                  .exists(uri => uri.length > 8192 || filePath(uri).isEmpty)
              )
                failure = Some("INVALID_BSP_OUTPUT_URI")
              else {
                val row = new JsonArray()
                row.add(current.getUri())
                val deps = new JsonArray()
                dependencies.foreach(dep => deps.add(dep.getUri()))
                row.add(deps)
                val roots = new JsonArray()
                directories.foreach { uri =>
                  roots.add(uri)
                  outputs += Output(current.getUri(), uri)
                }
                row.add(roots)
                rows.add(row)
                if (rows.toString.length > 128 * 1024)
                  failure = Some("BSP_OUTPUT_METADATA_BUDGET")
                pending = (pending ++ dependencies).sortBy(_.getUri())
              }
          }
      }
    }
    failure match {
      case Some(reason) => Left(reason)
      case None => Right(Snapshot(outputs.result(), rows.toString))
    }
  }

  def correlate(
      result: JsonObject,
      before: Either[String, Snapshot],
      after: Either[String, Snapshot],
  ): JsonObject = (before, after) match {
    case (Left(reason), _) => unavailable(reason)
    case (_, Left(reason)) => unavailable(reason)
    case (Right(first), Right(last)) if first != last =>
      unavailable("BSP_OUTPUT_METADATA_CHANGED")
    case (Right(snapshot), Right(_)) =>
      val index = outputIndex(snapshot.outputs)
      result.addProperty("schemaVersion", SchemaVersion)
      result.add("outputMetadata", JsonParser.parseString(snapshot.metadata))
      result.addProperty(
        "outputMetadataSha256",
        JavaReceiverTypes.optionsHash(List(snapshot.metadata)),
      )
      result.addProperty("compilationGenerationStatus", "NOT_RETAINED")
      result.getAsJsonArray("documents").asScala.foreach { document =>
        document.getAsJsonObject
          .getAsJsonArray("receiverSites")
          .asScala
          .foreach { site =>
            val row = site.getAsJsonObject
            if (row.has("selectedOrigin")) {
              val origin = row.getAsJsonObject("selectedOrigin")
              if (origin.get("status").getAsString == "CLASSFILE_ORIGIN") {
                index.flatMap(
                  indexedOutputOwner(origin.get("uri").getAsString, _)
                ) match {
                  case Left(reason) =>
                    origin.addProperty("outputStatus", reason)
                  case Right(owner) =>
                    origin.addProperty("outputStatus", "BSP_OUTPUT_MEMBER")
                    origin.addProperty("outputBuildTargetId", owner.targetId)
                    origin.addProperty("outputDirectoryUri", owner.uri)
                }
              }
            }
          }
      }
      result
  }
}
