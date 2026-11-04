package scala.meta.internal.metals.logging

import java.io.IOException
import java.io.PrintStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.RecursivelyDelete
import scala.meta.io.AbsolutePath

import tests.BaseSuite

class RuntimeLogSuite extends BaseSuite {
  test(
    "real-logger-wiring-rotates-and-preserves-first-incidents-in-a-subprocess"
  ) {
    val root = Files.createTempDirectory("metals-runtime-logger-wiring")
    val java = Paths.get(
      System.getProperty("java.home"),
      "bin",
      if (scala.util.Properties.isWin) "java.exe" else "java",
    )
    val process = new ProcessBuilder(
      java.toString,
      "-cp",
      System.getProperty("java.class.path"),
      "scala.meta.internal.metals.logging.RuntimeLogProbe",
      root.toString,
    ).redirectErrorStream(true)
      .redirectOutput(root.resolve("subprocess.log").toFile)
      .start()
    try {
      assert(
        process.waitFor(15, TimeUnit.SECONDS),
        "logger subprocess timed out",
      )
      assertEquals(process.exitValue(), 0)
      val path = root.resolve(".metals/metals.log")
      assertEquals(chunks(path).size, 3)
      assert(chunks(path).forall(Files.size(_) <= 128))
      assert(
        Files
          .readString(path.resolveSibling("metals.log.first-assertion"))
          .startsWith("AssertionError: first-live-assertion")
      )
      assert(
        Files
          .readString(path.resolveSibling("metals.log.first-error"))
          .contains("first-live-error")
      )
      assert(
        Files.size(path.resolveSibling("metals.log.first-assertion")) <= 128
      )
      assert(Files.size(path.resolveSibling("metals.log.first-error")) <= 128)
      assert(
        Files
          .readString(path.resolveSibling("metals.log.rotation"))
          .contains("samples-may-be-truncated=true")
      )
      assert(
        Files
          .readString(path.resolveSibling("metals.log.samples"))
          .contains("complete-stack-not-guaranteed=true")
      )
    } finally {
      if (process.isAlive) {
        process.destroyForcibly()
        process.waitFor(15, TimeUnit.SECONDS)
      }
      RecursivelyDelete(AbsolutePath(root))
    }
  }

  private def withLog(f: (Path, RuntimeLogOutputStream) => Unit): Unit = {
    val directory = Files.createTempDirectory("metals-runtime-log")
    val path = directory.resolve("metals.log")
    val stream = new RuntimeLogOutputStream(path, 128, 2)
    try f(path, stream)
    finally {
      stream.close()
      val files = Files.list(directory)
      try files.forEach(file => Files.delete(file))
      finally files.close()
      Files.delete(directory)
    }
  }

  private def chunks(path: Path): List[Path] =
    List(".2", ".1", "")
      .map(suffix => path.resolveSibling(path.getFileName.toString + suffix))
      .filter(Files.exists(_))

  test("oversized-single-write-and-one-byte-writes-are-bounded") {
    withLog { (path, stream) =>
      stream.write(Array.fill[Byte](4097)('x'.toByte))
      for (_ <- 1 to 513) stream.write('y')
      stream.flush()
      assertEquals(chunks(path).size, 3)
      assert(chunks(path).forall(Files.size(_) <= 128))
      assert(Files.readString(path).forall(_ == 'y'))
      val marker = Files.readString(path.resolveSibling("metals.log.rotation"))
      assert(marker.contains("runtime-log-v2 rotations=36"))
      assert(marker.contains("older-chunks-may-be-evicted=true"))
      assert(marker.getBytes(UTF_8).length < 256)
    }
  }

  test("scribe-and-compiler-stderr-share-rotation-and-stay-writable") {
    withLog { (path, stream) =>
      val raw = new PrintStream(stream, true, UTF_8)
      val logger = scribe.Logger.root
        .clearHandlers()
        .withHandler(
          writer = MetalsLogger.streamWriter(stream),
          formatter = MetalsLogger.defaultFormat,
        )
      raw.print("compiler-assertion\n" * 200)
      logger.info("selected-callable-after-assertion")
      raw.print("compiler-tail\n")
      assert(!raw.checkError())
      assert(chunks(path).forall(Files.size(_) <= 128))
      val retained = chunks(path).map(Files.readString(_)).mkString
      assert(retained.contains("selected-callable-after-assertion"))
      assert(retained.endsWith("compiler-tail\n"))
    }
  }

  test("concurrent-writers-obey-one-byte-budget") {
    withLog { (path, stream) =>
      val failures = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
      val threads = (1 to 4).map { _ =>
        new Thread(() => {
          try for (_ <- 1 to 100) stream.write(Array.fill[Byte](57)(1))
          catch { case error: Throwable => failures.add(error); () }
        })
      }
      threads.foreach(_.start())
      threads.foreach(_.join())
      assert(failures.isEmpty, s"writer failures: $failures")
      assert(chunks(path).forall(Files.size(_) <= 128))
      assertEquals(Files.size(path), 22800L % 128)
    }
  }

  test("closed-stream-and-invalid-ranges-fail") {
    withLog { (_, stream) =>
      intercept[IndexOutOfBoundsException](stream.write(Array[Byte](1), 1, 1))
      stream.close()
      intercept[IOException](stream.write(1))
      intercept[IOException](stream.flush())
    }
  }

  test("zero-backups-retains-only-current-chunk") {
    withLog { (path, initial) =>
      initial.close()
      val stream = new RuntimeLogOutputStream(path, 128, 0)
      try stream.write(Array.fill[Byte](1025)(1))
      finally stream.close()
      assertEquals(chunks(path).size, 1)
      assertEquals(Files.size(path), 1L)
    }
  }

  test("first-assertion-and-first-error-survive-later-log-storms") {
    withLog { (path, stream) =>
      stream.write(("ordinary startup\n" * 100).getBytes(UTF_8))
      stream.write("java.lang.Asser".getBytes(UTF_8))
      stream.write(
        "tionError: original-compiler-failure\nfirst-stack\n".getBytes(UTF_8)
      )
      val logger = scribe.Logger.root
        .clearHandlers()
        .withHandler(
          writer = MetalsLogger.streamWriter(stream),
          formatter = MetalsLogger.defaultFormat,
        )
      logger.error("first-structured-error")
      stream.write(("AssertionError: repeated-failure\n" * 200).getBytes(UTF_8))
      logger.error("later-structured-error")
      stream.close()
      val assertion = path.resolveSibling("metals.log.first-assertion")
      val error = path.resolveSibling("metals.log.first-error")
      assert(
        Files
          .readString(assertion)
          .startsWith(
            "AssertionError: original-compiler-failure\nfirst-stack\n"
          )
      )
      assertEquals(Files.size(assertion), 128L)
      assert(Files.readString(error).contains("first-structured-error"))
      assert(!Files.readString(error).contains("later-structured-error"))
      assert(Files.size(error) <= 128)
      assert(Files.size(path.resolveSibling("metals.log.samples")) < 256)
      assert(chunks(path).forall(Files.size(_) <= 128))
      assert(
        !chunks(path)
          .map(Files.readString(_))
          .mkString
          .contains("original-compiler-failure")
      )
    }
  }

  test("existing-first-incident-samples-are-never-overwritten") {
    withLog { (path, stream) =>
      val assertion = path.resolveSibling("metals.log.first-assertion")
      val error = path.resolveSibling("metals.log.first-error")
      Files.writeString(assertion, "original assertion")
      Files.writeString(error, "original error")
      stream.write("AssertionError: new\n".getBytes(UTF_8))
      stream.writeErrorRecord("new error".getBytes(UTF_8))
      assertEquals(Files.readString(assertion), "original assertion")
      assertEquals(Files.readString(error), "original error")
    }
  }
}

/** Separate JVM so setting the process streams cannot affect another test. */
object RuntimeLogProbe {
  def main(args: Array[String]): Unit = {
    MetalsLogger.setupLspLogger(
      List(AbsolutePath(Paths.get(args(0)))),
      redirectSystemStreams = true,
      MetalsServerConfig.default.copy(maxLogFileSize = 128, maxLogBackups = 2),
    )
    System.err.print("ordinary logging\n" * 200)
    System.err.print("java.lang.Asser")
    System.err.print("tionError: first-live-assertion\nfirst-stack\n")
    scribe.error("first-live-error")
    System.err.print("AssertionError: repeated\n" * 200)
    scribe.error("later-live-error")
    System.out.close()
  }
}
