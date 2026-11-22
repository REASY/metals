package scala.meta.internal.metals

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.pc.CancelToken

import com.google.gson.JsonArray
import com.google.gson.JsonObject

/** Bounded whole-context checks. Paths are locations, never package identities. */
object JavaDependencyContext {
  private val MaxEntries = 65536
  private val MaxBytes = 1024L * 1024 * 1024
  private case class Rejected(reason: String) extends RuntimeException
  private case class Snapshot(rows: JsonArray, hash: String)

  private def hex(bytes: Array[Byte]): String =
    "sha256:" + bytes.map(b => f"${b & 0xff}%02x").mkString
  private def hash(text: String): String =
    hex(MessageDigest.getInstance("SHA-256").digest(text.getBytes(UTF_8)))

  private def capture(
      classpath: Seq[Path],
      token: CancelToken,
  ): Either[String, Snapshot] =
    try {
      var entries = 0
      var bytes = 0L
      var nameBytes = 0L
      def memberName(root: Path, path: Path): String = {
        val relative = root.relativize(path)
        val name = relative.toString
        nameBytes += name.getBytes(UTF_8).length
        if (nameBytes > 8 * 1024 * 1024 || name.length > 8192)
          throw Rejected("CONTEXT_ENTRY_BUDGET")
        if (Paths.get(name) != relative || name.exists(_.isControl))
          throw Rejected("CONTEXT_UNSUPPORTED_ENTRY")
        name
      }
      def count(): Unit = {
        token.checkCanceled()
        entries += 1
        if (entries > MaxEntries) throw Rejected("CONTEXT_ENTRY_BUDGET")
      }
      def fileHash(path: Path, before: BasicFileAttributes): String = {
        if (!before.isRegularFile || before.isSymbolicLink)
          throw Rejected("CONTEXT_UNSUPPORTED_ENTRY")
        if (before.size > MaxBytes - bytes)
          throw Rejected("CONTEXT_BYTE_BUDGET")
        val md = MessageDigest.getInstance("SHA-256")
        val stream = Files.newInputStream(path, LinkOption.NOFOLLOW_LINKS)
        var seen = 0L
        try {
          val buffer = new Array[Byte](64 * 1024)
          var read = stream.read(buffer)
          while (read >= 0) {
            token.checkCanceled()
            seen += read
            bytes += read
            if (bytes > MaxBytes) throw Rejected("CONTEXT_BYTE_BUDGET")
            md.update(buffer, 0, read)
            read = stream.read(buffer)
          }
        } finally stream.close()
        val after = Files.readAttributes(
          path,
          classOf[BasicFileAttributes],
          LinkOption.NOFOLLOW_LINKS,
        )
        if (
          !after.isRegularFile || seen != before.size ||
          before.size != after.size ||
          before.lastModifiedTime != after.lastModifiedTime ||
          before.fileKey != after.fileKey
        ) throw Rejected("CONTEXT_CHANGED")
        hex(md.digest())
      }
      val rows = new JsonArray()
      classpath.foreach { raw =>
        val root = raw.toAbsolutePath.normalize()
        val startEntries = entries
        val startBytes = bytes
        val attrs = Files.readAttributes(
          root,
          classOf[BasicFileAttributes],
          LinkOption.NOFOLLOW_LINKS,
        )
        val row = new JsonArray()
        row.add(root.toString)
        if (attrs.isDirectory) {
          val members = mutable.ArrayBuffer.empty[(String, String)]
          Files.walkFileTree(
            root,
            new SimpleFileVisitor[Path] {
              override def preVisitDirectory(
                  dir: Path,
                  attrs: BasicFileAttributes,
              ): FileVisitResult = {
                count()
                members += ((memberName(root, dir), "DIRECTORY"))
                FileVisitResult.CONTINUE
              }
              override def visitFile(
                  file: Path,
                  attrs: BasicFileAttributes,
              ): FileVisitResult = {
                count()
                members += ((
                  memberName(root, file),
                  fileHash(file, attrs),
                ))
                FileVisitResult.CONTINUE
              }
            },
          )
          val contents = new JsonArray()
          members.sortBy(_._1).foreach { case (name, digest) =>
            val item = new JsonArray()
            item.add(name)
            item.add(digest)
            contents.add(item)
          }
          row.add("DIRECTORY")
          row.add(hash(contents.toString))
        } else {
          count()
          row.add("FILE")
          row.add(fileHash(root, attrs))
        }
        row.add(entries - startEntries)
        row.add(bytes - startBytes)
        rows.add(row)
      }
      token.checkCanceled()
      Right(Snapshot(rows, hash(rows.toString)))
    } catch {
      case Rejected(reason) => Left(reason)
      case _: IOException => Left("CONTEXT_IO_UNAVAILABLE")
      case _: SecurityException => Left("CONTEXT_IO_UNAVAILABLE")
    }

  def run(
      params: JavaDependencyBodiesParams,
      classpath: Seq[Path],
      source: String,
      token: CancelToken,
  )(acquire: => Future[JsonObject])(implicit
      ec: ExecutionContext
  ): Future[JsonObject] = {
    def unavailable(reason: String) = JavaDependencyBodies.unavailable(reason)
    Future {
      if (hash(source) != params.sourceSha256)
        Left("TARGET_SOURCE_MISMATCH")
      else capture(classpath, token)
    }.flatMap {
      case Left(reason) => Future.successful(unavailable(reason))
      case Right(before) if params.phase == "FREEZE" =>
        val result = unavailable("CONTEXT_READY")
        result.addProperty("sourceSha256", params.sourceSha256)
        result.addProperty("uri", params.uri)
        result.addProperty("buildTargetId", params.buildTargetId)
        result.add("compilerContent", before.rows)
        result.addProperty("compilerContentSha256", before.hash)
        Future.successful(result)
      case Right(before) if before.hash != params.compilerContentSha256 =>
        Future.successful(unavailable("CONTEXT_CHANGED"))
      case Right(before) =>
        acquire.map { result =>
          capture(classpath, token) match {
            case Left(reason) => unavailable(reason)
            case Right(after) if after != before =>
              unavailable("CONTEXT_CHANGED")
            case Right(_) =>
              result.add("compilerContent", before.rows)
              result.addProperty("compilerContentSha256", before.hash)
              result
          }
        }
    }
  }

  /** One request-owned, before/after classpath snapshot for compiled origins. */
  def runCompiled(
      sourceSha256: String,
      classpath: Seq[Path],
      source: String,
      token: CancelToken,
  )(acquire: => Future[JsonObject])(implicit
      ec: ExecutionContext
  ): Future[JsonObject] = {
    def unavailable(reason: String) = JavaCompiledOrigins.unavailable(reason)
    val paths = classpath.map(_.toAbsolutePath.normalize().toString)
    if (paths.size > 4096 || paths.map(_.length.toLong).sum > 128 * 1024)
      Future.successful(unavailable("CLASSPATH_IDENTITY_BUDGET"))
    else
      Future {
        if (hash(source) != sourceSha256) Left("TARGET_SOURCE_MISMATCH")
        else capture(classpath, token)
      }.flatMap {
        case Left(reason) => Future.successful(unavailable(reason))
        case Right(before) =>
          acquire.map { result =>
            capture(classpath, token) match {
              case Left(reason) => unavailable(reason)
              case Right(after) if after != before =>
                unavailable("CONTEXT_CHANGED")
              case Right(_) =>
                val values = new JsonArray()
                paths.foreach(values.add)
                result.add("compilerClasspath", values)
                result.addProperty(
                  "compilerClasspathSha256",
                  JavaReceiverTypes.optionsHash(paths.toList),
                )
                result.add("compilerContent", before.rows)
                result.addProperty("compilerContentSha256", before.hash)
                result.addProperty(
                  "compilerContentStatus",
                  "UNCHANGED_DURING_ACQUISITION",
                )
                result
            }
          }
      }
  }
}
