package scala.meta.internal.metals

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

import munit.FunSuite
import org.eclipse.lsp4j.Location

class DefinitionReadinessSuite extends FunSuite {
  test("non-empty definition does not wait for global indexing") {
    val indexingReady = Promise[Unit]()
    val retryCount = new AtomicInteger()
    val location = new Location()
    val result = DefinitionResult(
      List(location).asJava,
      "example/Target#",
      None,
      None,
      "example/Target#",
    )

    val completed = DefinitionResult.retryEmptyAfter(
      Future.successful(result),
      Some(indexingReady.future),
    ) {
      retryCount.incrementAndGet()
      Future.successful(DefinitionResult.empty)
    }

    assertEquals(Await.result(completed, 5.seconds), result)
    assertEquals(retryCount.get(), 0)
    assert(!indexingReady.isCompleted)
  }

  test("empty definition retries after global indexing") {
    val indexingReady = Promise[Unit]()
    val retryCount = new AtomicInteger()
    val location = new Location()
    val retried = DefinitionResult(
      List(location).asJava,
      "example/Dependency#get().",
      None,
      None,
      "example/Dependency#get().",
    )

    val completed = DefinitionResult.retryEmptyAfter(
      Future.successful(DefinitionResult.empty),
      Some(indexingReady.future),
    ) {
      retryCount.incrementAndGet()
      Future.successful(retried)
    }
    assert(!completed.isCompleted)
    assertEquals(retryCount.get(), 0)

    indexingReady.success(())
    assertEquals(Await.result(completed, 5.seconds), retried)
    assertEquals(retryCount.get(), 1)
  }

  test("empty definition after global indexing does not retry") {
    val retryCount = new AtomicInteger()
    val completed = DefinitionResult.retryEmptyAfter(
      Future.successful(DefinitionResult.empty),
      None,
    ) {
      retryCount.incrementAndGet()
      Future.successful(DefinitionResult.empty)
    }

    assert(Await.result(completed, 5.seconds).isEmpty)
    assertEquals(retryCount.get(), 0)
  }
}
