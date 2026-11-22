package scala.meta.internal.metals.logging

import java.io.IOException
import java.io.OutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.StandardOpenOption.APPEND
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.CREATE_NEW
import java.nio.file.StandardOpenOption.WRITE

/**
 * One synchronized sink must be shared by Scribe and redirected compiler output.
 * Rotating only the Scribe writer leaves System.err writing an unbounded inode.
 * The sidecar makes rotation/diagnostic eviction visible without recursively
 * logging through this stream. Rotation bounds bytes, not compiler CPU usage.
 */
final class RuntimeLogOutputStream(
    path: Path,
    maxBytes: Long,
    backups: Int,
) extends OutputStream {
  require(maxBytes > 0, "runtime log byte limit must be positive")
  require(backups >= 0, "runtime log backup count must be non-negative")
  Files.createDirectories(path.getParent)
  private var output = Files.newOutputStream(path, CREATE, APPEND)
  private var size = Files.size(path)
  private var rotations = 0L
  private var closed = false
  private val sampleLimit = math.min(maxBytes, 64L * 1024).toInt
  private val assertionMarker = "AssertionError".getBytes(UTF_8)
  private var markerPosition = 0
  private var assertionSeen = false
  private var assertionSample: Option[OutputStream] = None
  private var assertionSampleBytes = 0
  private var errorSeen = false

  private def sibling(suffix: String): Path =
    path.resolveSibling(path.getFileName.toString + suffix)

  private def newSample(suffix: String): Option[OutputStream] = {
    try
      Files.writeString(
        sibling(".samples"),
        s"runtime-log-samples-v1 prefix-only=true max-bytes-per-sample=$sampleLimit complete-stack-not-guaranteed=true raw-assertion-is-text-detection=true\n",
        CREATE_NEW,
        WRITE,
      )
    catch { case _: FileAlreadyExistsException => () }
    try Some(Files.newOutputStream(sibling(suffix), CREATE_NEW, WRITE))
    catch { case _: FileAlreadyExistsException => None }
  }

  /** Preserve the first structured error separately from the rotating tail. */
  private[metals] def writeErrorRecord(bytes: Array[Byte]): Unit =
    synchronized {
      if (closed) throw new IOException("runtime log stream is closed")
      if (!errorSeen) {
        errorSeen = true
        newSample(".first-error").foreach { sample =>
          try sample.write(bytes, 0, math.min(bytes.length, sampleLimit))
          finally sample.close()
        }
      }
      write(bytes)
    }

  private def appendAssertionSample(
      bytes: Array[Byte],
      offset: Int,
      length: Int,
  ): Unit = {
    assertionSample.foreach { sample =>
      val count = math.min(length, sampleLimit - assertionSampleBytes)
      sample.write(bytes, offset, count)
      assertionSampleBytes += count
      if (assertionSampleBytes == sampleLimit) {
        sample.close()
        assertionSample = None
      }
    }
  }

  /**
   * Raw compiler stderr has no LogRecord. Retain the first bounded byte window
   * beginning with AssertionError, including markers split across writes.
   * This is diagnostic text detection, not an exception/semantic classification.
   */
  private def retainFirstAssertion(
      bytes: Array[Byte],
      offset: Int,
      length: Int,
  ): Unit = {
    var cursor = offset
    val end = offset + length
    while (!assertionSeen && cursor < end) {
      val next = bytes(cursor)
      if (next == assertionMarker(markerPosition)) markerPosition += 1
      else markerPosition = if (next == assertionMarker(0)) 1 else 0
      cursor += 1
      if (markerPosition == assertionMarker.length) {
        assertionSeen = true
        assertionSample = newSample(".first-assertion")
        appendAssertionSample(assertionMarker, 0, assertionMarker.length)
      }
    }
    appendAssertionSample(bytes, cursor, end - cursor)
  }

  private def rotate(): Unit = {
    output.close()
    // If a filesystem operation fails, do not continue writing through a stale
    // descriptor or silently reset accounting. A later call must still fail.
    closed = true
    for (number <- backups until 1 by -1) {
      val previous = sibling(s".${number - 1}")
      if (Files.exists(previous))
        Files.move(previous, sibling(s".$number"), REPLACE_EXISTING)
    }
    if (backups > 0) Files.move(path, sibling(".1"), REPLACE_EXISTING)
    else Files.delete(path)
    rotations += 1
    Files.write(
      sibling(".rotation"),
      s"runtime-log-v2 rotations=$rotations max-bytes=$maxBytes backups=$backups sample-limit=$sampleLimit older-chunks-may-be-evicted=true samples-may-be-truncated=true\n"
        .getBytes(UTF_8),
    )
    output = Files.newOutputStream(path, CREATE, APPEND)
    size = 0L
    closed = false
  }

  override def write(value: Int): Unit = write(Array(value.toByte), 0, 1)

  override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
    synchronized {
      if (closed) throw new IOException("runtime log stream is closed")
      if (offset < 0 || length < 0 || offset > bytes.length - length)
        throw new IndexOutOfBoundsException()
      retainFirstAssertion(bytes, offset, length)
      var position = offset
      var remaining = length
      while (remaining > 0) {
        if (size >= maxBytes) rotate()
        val count = math.min(remaining.toLong, maxBytes - size).toInt
        output.write(bytes, position, count)
        size += count
        position += count
        remaining -= count
      }
    }

  override def flush(): Unit = synchronized {
    if (closed) throw new IOException("runtime log stream is closed")
    output.flush()
  }

  override def close(): Unit = synchronized {
    assertionSample.foreach(_.close())
    assertionSample = None
    if (!closed) {
      closed = true
      output.close()
    }
  }
}
