package scala.meta.internal.metals

import java.lang.ref.WeakReference
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.CREATE_NEW
import java.nio.file.StandardOpenOption.WRITE
import java.util.WeakHashMap

import scala.meta.internal.metals.utils.LimitedFilesManager
import scala.meta.internal.metals.utils.TimestampedFile

/**
 * One bounded inventory per reporter directory, shared by contexts in this JVM.
 * Directory discovery is initialization work, never per-report work. Runtime
 * retention preserves the earliest incident and the most recent reports;
 * explicit cleanup/deletion still has its existing newest-only semantics.
 */
private[metals] final class ReportFileRetention(directory: Path) {
  import ReportFileRetention._

  private var initialized = false
  private var files = Vector.empty[TimestampedFile]
  private var ids = Map.empty[String, Path]
  private var lastTimestamp = 0L
  private var evicted = 0L
  private var truncated = 0L
  private val statusPath = directory.resolve("retention-status.txt")

  def initialize(manager: LimitedFilesManager): Unit = synchronized {
    if (!initialized) {
      files = manager.getAllFiles().sortBy(f => (f.timestamp, f.name)).toVector
      prune(StdReportContext.MAX_NUMBER_OF_REPORTS, preserveFirst = true)
      files.foreach { report =>
        val path = report.toPath
        if (Files.size(path) > StdReportContext.MAX_REPORT_BYTES) {
          val prefix = readPrefix(path, StdReportContext.MAX_REPORT_BYTES + 1)
          Files.write(path, bounded(prefix))
          truncated += 1
        }
        val prefix = readPrefix(path, MaxIdBytes + Report.idPrefix.length + 1)
        val firstLine = new String(prefix, UTF_8).takeWhile(_ != '\n')
        if (
          firstLine.startsWith(Report.idPrefix) &&
          prefix.contains('\n'.toByte)
        ) {
          boundedId(Some(firstLine.stripPrefix(Report.idPrefix)))
            .foreach(id => ids += (id -> path))
        }
      }
      lastTimestamp = files.lastOption.fold(0L)(_.timestamp)
      if (evicted > 0 || truncated > 0) writeStatus()
      initialized = true
    }
  }

  def create(id: Option[String], pathFor: Long => Path)(
      text: => String
  ): (Path, Boolean) = synchronized {
    val key = boundedId(id)
    val duplicate = key.flatMap(ids.get).filter(Files.isRegularFile(_))
    duplicate match {
      case Some(path) => (path, false)
      case None =>
        // Reserve disk before writing, including on repeated failures.
        prune(StdReportContext.MAX_NUMBER_OF_REPORTS - 1, preserveFirst = true)
        val raw =
          text.take(StdReportContext.MAX_REPORT_BYTES + 1).getBytes(UTF_8)
        val bytes = bounded(raw)
        lastTimestamp = math.max(System.currentTimeMillis(), lastTimestamp + 1)
        val path = pathFor(lastTimestamp)
        Files.createDirectories(path.getParent)
        // No replacement: distinct incidents must never overwrite one another.
        val output = Files.newOutputStream(path, CREATE_NEW, WRITE)
        // Track even a failed partial write so retries cannot leak files.
        files :+= TimestampedFile(path.toFile, lastTimestamp)
        try output.write(bytes)
        finally output.close()
        key.foreach(id => ids += (id -> path))
        if (raw.length > StdReportContext.MAX_REPORT_BYTES) truncated += 1
        if (evicted > 0 || truncated > 0) writeStatus()
        (path, true)
    }
  }

  def reports: List[TimestampedFile] = synchronized { files.toList }

  def cleanup(limit: Int): List[TimestampedFile] = synchronized {
    val removed = prune(math.max(0, limit), preserveFirst = false)
    if (removed.nonEmpty) writeStatus()
    removed
  }

  def deleteAll(): Unit = synchronized {
    prune(0, preserveFirst = false)
    Files.deleteIfExists(statusPath)
    ids = Map.empty
    evicted = 0L
    truncated = 0L
    initialized = false
  }

  private def prune(
      limit: Int,
      preserveFirst: Boolean
  ): List[TimestampedFile] = {
    val start = if (preserveFirst && limit > 0) 1 else 0
    val removed = files.slice(start, start + math.max(0, files.size - limit))
    var deleted = Set.empty[Path]
    try
      removed.foreach { report =>
        Files.deleteIfExists(report.toPath)
        deleted += report.toPath
        evicted += 1
      }
    finally {
      files = files.filterNot(file => deleted(file.toPath))
      ids = ids.filterNot(entry => deleted(entry._2))
    }
    removed.toList
  }

  private def writeStatus(): Unit = {
    Files.createDirectories(directory)
    Files.write(
      statusPath,
      s"format=metals-report-retention.v1\nevicted=$evicted\ntruncated=$truncated\nmaxReports=${StdReportContext.MAX_NUMBER_OF_REPORTS}\nmaxReportBytes=${StdReportContext.MAX_REPORT_BYTES}\npreservesFirstIncident=true\n"
        .getBytes(UTF_8)
    )
  }
}

private[metals] object ReportFileRetention {
  private val stores =
    new WeakHashMap[Path, WeakReference[ReportFileRetention]]()
  private val MaxIdBytes = 2048
  private val marker =
    "\n\n[metals-report-retention.v1: report truncated at byte limit]\n"
      .getBytes(UTF_8)

  def forDirectory(path: Path): ReportFileRetention = synchronized {
    val key = path.toAbsolutePath.normalize()
    Option(stores.get(key)).flatMap(ref => Option(ref.get())) match {
      case Some(store) => store
      case None =>
        val store = new ReportFileRetention(key)
        stores.put(key, new WeakReference(store))
        store
    }
  }

  private def boundedId(id: Option[String]): Option[String] =
    id.filter(value =>
      value.length <= MaxIdBytes && !value.contains('\n') &&
        value.getBytes(UTF_8).length <= MaxIdBytes
    )

  private def readPrefix(path: Path, limit: Int): Array[Byte] = {
    val input = Files.newInputStream(path)
    val bytes = new Array[Byte](limit)
    var size = 0
    try {
      var count = input.read(bytes, size, limit - size)
      while (count > 0 && size < limit) {
        size += count
        count = if (size < limit) input.read(bytes, size, limit - size) else -1
      }
    } finally input.close()
    bytes.take(size)
  }

  private def bounded(bytes: Array[Byte]): Array[Byte] = {
    if (bytes.length <= StdReportContext.MAX_REPORT_BYTES) bytes
    else {
      var end = StdReportContext.MAX_REPORT_BYTES - marker.length
      // Do not split a UTF-8 code point before appending the ASCII marker.
      while (end > 0 && (bytes(end) & 0xc0) == 0x80) end -= 1
      bytes.take(end) ++ marker
    }
  }
}
