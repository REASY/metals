package scala.meta.internal.metals

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

import munit.FunSuite

class OpenedFileSemanticReadinessSuite extends FunSuite {
  test("definition waits for pending did-open semantic readiness") {
    val readiness = new OpenedFileSemanticReadiness[String]()
    val initialized = Promise[Unit]()
    val tracked = readiness.track("A.scala", initialized.future)
    val definitionStarted = new AtomicInteger()

    val definition = readiness.await("A.scala").map { _ =>
      definitionStarted.incrementAndGet()
    }
    assertEquals(definitionStarted.get(), 0)

    initialized.success(())
    Await.result(tracked, 5.seconds)
    Await.result(definition, 5.seconds)
    assertEquals(definitionStarted.get(), 1)
  }

  test("completed did-open failure remains visible until close") {
    val readiness = new OpenedFileSemanticReadiness[String]()
    val failure = new IllegalStateException("semantic initialization failed")
    Await.ready(
      readiness.track("A.scala", Future.failed(failure)),
      5.seconds,
    )

    val observed = intercept[IllegalStateException] {
      Await.result(readiness.await("A.scala"), 5.seconds)
    }
    assertEquals(observed, failure)

    readiness.remove("A.scala")
    Await.result(readiness.await("A.scala"), 5.seconds)
  }
}
