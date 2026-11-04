package tests

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.JavaDependencyBodiesParams
import scala.meta.internal.metals.JavaDependencyContext
import scala.meta.internal.metals.JavaReceiverRange
import scala.meta.pc.CancelToken

import com.google.gson.JsonObject

class JavaDependencyContextSuite extends munit.FunSuite {
  private val params = JavaDependencyBodiesParams(
    "file:///fixture/Caller.java",
    "file:///fixture?id=app",
    "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    java.util.List.of(JavaReceiverRange(0, 1)),
    65536,
    null,
    "FREEZE",
  )

  private def run(p: JavaDependencyBodiesParams, paths: Seq[Path])(
      body: => Future[JsonObject]
  ): Future[JsonObject] =
    JavaDependencyContext.run(p, paths, "", EmptyCancelToken)(body)

  test("changed-compiled-directory-before-and-during-acquisition") {
    val dir = Files.createTempDirectory("java-dependency-context")
    val file = dir.resolve("Api.class")
    Files.write(file, Array[Byte](1, 2, 3))
    var acquired = 0
    def body = {
      acquired += 1
      Future.successful(new JsonObject())
    }
    val check = for {
      frozen <- run(params, List(dir))(body)
      request = params.copy(
        phase = "ACQUIRE",
        compilerContentSha256 = frozen.get("compilerContentSha256").getAsString,
      )
      _ = Files.write(file, Array[Byte](4, 5, 6)) // same path and size
      before <- run(request, List(dir))(body)
      _ = assertEquals(acquired, 0)
      _ = Files.write(file, Array[Byte](1, 2, 3))
      during <- run(request, List(dir)) {
        Files.write(file, Array[Byte](7, 8, 9))
        body
      }
    } yield {
      assertEquals(before.get("status").getAsString, "CONTEXT_CHANGED")
      assertEquals(during.get("status").getAsString, "CONTEXT_CHANGED")
      assertEquals(acquired, 1)
    }
    check.andThen { case _ =>
      Files.deleteIfExists(file)
      Files.deleteIfExists(dir)
    }
  }

  test("symlink-is-not-followed") {
    val dir = Files.createTempDirectory("java-dependency-context-link")
    val file = dir.resolve("Api.class")
    val link = dir.resolve("Link.class")
    Files.write(file, Array[Byte](1))
    Files.createSymbolicLink(link, file.getFileName)
    run(params, List(dir))(
      Future.failed(new AssertionError("must not acquire"))
    )
      .map(r =>
        assertEquals(r.get("status").getAsString, "CONTEXT_UNSUPPORTED_ENTRY")
      )
      .andThen { case _ =>
        Files.deleteIfExists(link)
        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
      }
  }

  test("missing-input-is-explicit") {
    val dir = Files.createTempDirectory("java-dependency-context-missing")
    run(params, List(dir.resolve("absent.jar")))(
      Future.failed(new AssertionError("must not acquire"))
    )
      .map(r =>
        assertEquals(r.get("status").getAsString, "CONTEXT_IO_UNAVAILABLE")
      )
      .andThen { case _ => Files.deleteIfExists(dir) }
  }

  test("oversized-content-rejected-before-reading") {
    val dir = Files.createTempDirectory("java-dependency-context-budget")
    val file = dir.resolve("large.jar")
    val handle = new java.io.RandomAccessFile(file.toFile, "rw")
    try handle.setLength(1024L * 1024 * 1024 + 1)
    finally handle.close()
    run(params, List(file))(
      Future.failed(new AssertionError("must not acquire"))
    )
      .map(r =>
        assertEquals(r.get("status").getAsString, "CONTEXT_BYTE_BUDGET")
      )
      .andThen { case _ =>
        Files.deleteIfExists(file)
        Files.deleteIfExists(dir)
      }
  }

  test("cancellation-is-not-degradation") {
    val token = new CancelToken {
      override def checkCanceled(): Unit = throw new CancellationException()
      override def onCancel()
          : java.util.concurrent.CompletionStage[java.lang.Boolean] =
        CompletableFuture.completedFuture(java.lang.Boolean.TRUE)
    }
    JavaDependencyContext
      .run(params, Nil, "", token)(
        Future.failed(new AssertionError("must not acquire"))
      )
      .failed
      .map(error => assert(error.isInstanceOf[CancellationException]))
  }
}
