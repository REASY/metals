package scala.meta.internal.metals

import java.io.IOException
import java.net.URI
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.Base64
import java.util.concurrent.CancellationException

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

import scala.meta.pc.CancelToken

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

/** Exact bounded bytes for selected compiled workspace declarations. */
object JavaCompiledClassfiles {
  private val MaxClassBytes = 1024 * 1024
  private val MaxTotalBytes = 4 * 1024 * 1024

  def declaration(
      bytes: Array[Byte],
      selected: JsonObject,
      token: CancelToken,
  ): Either[String, JsonObject] = {
    if (bytes.length > MaxClassBytes) Left("CLASSFILE_BYTE_BUDGET")
    else {
      token.checkCanceled()
      try {
        val reader = new ClassReader(bytes)
        val owner = selected.get("binaryOwner").getAsString
        if (reader.getClassName.replace('/', '.') != owner)
          Left("CLASSFILE_OWNER_MISMATCH")
        else {
          val member = selected.get("member").getAsString
          val descriptor = selected.get("descriptor").getAsString
          val matches = mutable.ArrayBuffer.empty[(Int, Boolean)]
          reader.accept(
            new ClassVisitor(Opcodes.ASM9) {
              override def visitMethod(
                  access: Int,
                  name: String,
                  desc: String,
                  signature: String,
                  exceptions: Array[String],
              ): MethodVisitor =
                if (name != member || desc != descriptor) null
                else
                  new MethodVisitor(Opcodes.ASM9) {
                    private var hasCode = false
                    override def visitCode(): Unit = { hasCode = true }
                    override def visitEnd(): Unit = {
                      matches += ((access, hasCode))
                    }
                  }
            },
            ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES,
          )
          matches.toVector match {
            case Vector((access, hasCode)) =>
              val abstractMethod = (access & Opcodes.ACC_ABSTRACT) != 0
              val nativeMethod = (access & Opcodes.ACC_NATIVE) != 0
              if (abstractMethod != selected.get("abstract").getAsBoolean)
                Left("CLASSFILE_ABSTRACT_MISMATCH")
              else if (hasCode == (abstractMethod || nativeMethod))
                Left("CLASSFILE_BODY_FLAGS_MISMATCH")
              else {
                val row = new JsonObject()
                row.addProperty("binaryOwner", owner)
                row.add("member", selected.get("member"))
                row.add("descriptor", selected.get("descriptor"))
                row.addProperty("classAccess", reader.getAccess)
                row.addProperty("methodAccess", access)
                row.addProperty("hasCode", hasCode)
                row.addProperty(
                  "bodyKind",
                  if (abstractMethod) "ABSTRACT_CONTRACT"
                  else if (nativeMethod) "NATIVE_DECLARATION"
                  else "JVM_CODE",
                )
                Right(row)
              }
            case matches if matches.isEmpty =>
              Left("CLASSFILE_METHOD_UNAVAILABLE")
            case _ => Left("CLASSFILE_METHOD_AMBIGUOUS")
          }
        }
      } catch {
        case canceled: CancellationException => throw canceled
        case NonFatal(_) => Left("CLASSFILE_INVALID")
      }
    }
  }

  private def read(
      path: Path,
      output: Path,
      remaining: Int,
      token: CancelToken,
  ): Either[String, Array[Byte]] =
    try {
      token.checkCanceled()
      if (
        path == output || !path.startsWith(output) ||
        path.toRealPath() != path || output.toRealPath() != output ||
        !Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)
      ) Left("CLASSFILE_LOCATION_UNSUPPORTED")
      else if (Files.size(path) > math.min(MaxClassBytes, remaining))
        Left("CLASSFILE_BYTE_BUDGET")
      else {
        val stream = Files.newInputStream(path, LinkOption.NOFOLLOW_LINKS)
        val bytes =
          try stream.readNBytes(math.min(MaxClassBytes, remaining) + 1)
          finally stream.close()
        token.checkCanceled()
        if (bytes.length > math.min(MaxClassBytes, remaining))
          Left("CLASSFILE_BYTE_BUDGET")
        else Right(bytes)
      }
    } catch {
      case _: IOException | _: SecurityException =>
        Left("CLASSFILE_IO_UNAVAILABLE")
    }

  private def location(raw: String): Option[Path] =
    Try(URI.create(raw)).toOption
      .filter(uri =>
        uri.getScheme == "file" && uri.getRawQuery == null &&
          uri.getRawFragment == null &&
          Option(uri.getRawAuthority).forall(_.isEmpty)
      )
      .flatMap(uri => Try(Paths.get(uri).toAbsolutePath.normalize()).toOption)

  def enrich(
      result: JsonObject,
      classpath: Seq[Path],
      token: CancelToken,
  ): JsonObject = {
    val directories = classpath
      .map(_.toAbsolutePath.normalize())
      .filter(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
      .distinct
    val origins =
      mutable.Map.empty[String, Either[String, (String, Array[Byte])]]
    val retained = new JsonArray()
    var remaining = MaxTotalBytes
    result.getAsJsonArray("documents").asScala.foreach { document =>
      document.getAsJsonObject.getAsJsonArray("receiverSites").asScala.foreach {
        value =>
          token.checkCanceled()
          val row = value.getAsJsonObject
          if (row.has("selectedOrigin") && row.has("selectedCallable")) {
            val origin = row.getAsJsonObject("selectedOrigin")
            if (
              origin.has("outputStatus") &&
              origin.get("outputStatus").getAsString == "BSP_OUTPUT_MEMBER"
            ) {
              val uri = origin.get("uri").getAsString
              val content = origins.getOrElseUpdate(
                uri,
                (
                  location(uri),
                  location(origin.get("outputDirectoryUri").getAsString),
                ) match {
                  case (Some(path), Some(output))
                      if directories.exists(root =>
                        path != root && path.startsWith(root)
                      ) &&
                        retained.size() < 256 =>
                    read(path, output, remaining, token).map { bytes =>
                      remaining -= bytes.length
                      val hash = "sha256:" + MessageDigest
                        .getInstance("SHA-256")
                        .digest(bytes)
                        .map(b => f"${b & 0xff}%02x")
                        .mkString
                      val artifact = new JsonObject()
                      artifact.addProperty("uri", uri)
                      artifact.addProperty("sha256", hash)
                      artifact.addProperty(
                        "bytesBase64",
                        Base64.getEncoder.encodeToString(bytes),
                      )
                      retained.add(artifact)
                      (hash, bytes)
                    }
                  case _ => Left("CLASSFILE_OUTSIDE_COMPILER_CONTEXT")
                },
              )
              content match {
                case Left(reason) =>
                  origin.addProperty("classfileStatus", reason)
                case Right((hash, bytes)) =>
                  declaration(
                    bytes,
                    row.getAsJsonObject("selectedCallable"),
                    token,
                  ) match {
                    case Left(reason) =>
                      origin.addProperty("classfileStatus", reason)
                    case Right(proof) =>
                      proof.addProperty("classfileSha256", hash)
                      origin.addProperty("classfileStatus", "EXACT_DECLARATION")
                      origin.add("classfileDeclaration", proof)
                  }
              }
            }
          }
      }
    }
    result.add("compiledClasses", retained)
    result
  }
}
