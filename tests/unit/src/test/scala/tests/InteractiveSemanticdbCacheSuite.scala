package scala.meta.internal.metals

import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import munit.FunSuite

class InteractiveSemanticdbCacheSuite extends FunSuite {
  private val laneA = InteractiveSemanticdbCompilationLane.Target("target:A")
  private val laneB = InteractiveSemanticdbCompilationLane.Target("target:B")
  private val executor = Executors.newCachedThreadPool()
  private implicit val executionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(executor)

  override def afterAll(): Unit = executor.shutdownNow()

  test("same-source misses compile once") {
    val cache = new InteractiveSemanticdbCache[String, String]()
    val compileEntered = new CountDownLatch(1)
    val releaseCompile = new CountDownLatch(1)
    val compileCount = new AtomicInteger()

    def lookup() = Future {
      cache.compute("A.scala", laneA, _ == "current") { _ =>
        compileCount.incrementAndGet()
        compileEntered.countDown()
        releaseCompile.await()
        "current"
      }
    }

    val first = lookup()
    assert(compileEntered.await(5, TimeUnit.SECONDS))
    val second = lookup()
    releaseCompile.countDown()

    assertEquals(Await.result(first, 5.seconds), "current")
    assertEquals(Await.result(second, 5.seconds), "current")
    assertEquals(compileCount.get(), 1)
  }

  test("cache hit progresses while another source compiles") {
    val cache = new InteractiveSemanticdbCache[String, String]()
    cache.compute("B.scala", laneB, _ => false)(_ => "cached")
    val compileEntered = new CountDownLatch(1)
    val releaseCompile = new CountDownLatch(1)

    val miss = Future {
      cache.compute("A.scala", laneA, _ => false) { _ =>
        compileEntered.countDown()
        releaseCompile.await()
        "compiled"
      }
    }
    assert(compileEntered.await(5, TimeUnit.SECONDS))

    val hit = Future {
      cache.compute("B.scala", laneB, _ == "cached") { _ =>
        fail("a current cache entry must not compile")
      }
    }
    assertEquals(Await.result(hit, 5.seconds), "cached")

    releaseCompile.countDown()
    assertEquals(Await.result(miss, 5.seconds), "compiled")
  }

  test("different-source misses in one target serialize compiler access") {
    val cache = new InteractiveSemanticdbCache[String, String]()
    val compileEntered = new CountDownLatch(2)
    val releaseCompile = new CountDownLatch(1)
    val activeCompilations = new AtomicInteger()
    val maximumActiveCompilations = new AtomicInteger()

    def lookup(path: String) = Future {
      cache.compute(path, laneA, _ => false) { _ =>
        val active = activeCompilations.incrementAndGet()
        maximumActiveCompilations.accumulateAndGet(active, Math.max)
        compileEntered.countDown()
        releaseCompile.await()
        activeCompilations.decrementAndGet()
        path
      }
    }

    val first = lookup("A.scala")
    val second = lookup("B.scala")
    assert(!compileEntered.await(250, TimeUnit.MILLISECONDS))
    assertEquals(maximumActiveCompilations.get(), 1)

    releaseCompile.countDown()
    assertEquals(Await.result(first, 5.seconds), "A.scala")
    assertEquals(Await.result(second, 5.seconds), "B.scala")
    assertEquals(maximumActiveCompilations.get(), 1)
  }

  test("different-target misses compile concurrently") {
    val cache = new InteractiveSemanticdbCache[String, String]()
    val compileEntered = new CountDownLatch(2)
    val releaseCompile = new CountDownLatch(1)

    def lookup(path: String, lane: InteractiveSemanticdbCompilationLane) =
      Future {
        cache.compute(path, lane, _ => false) { _ =>
          compileEntered.countDown()
          releaseCompile.await()
          path
        }
      }

    val first = lookup("A.scala", laneA)
    val second = lookup("B.scala", laneB)
    assert(compileEntered.await(5, TimeUnit.SECONDS))

    releaseCompile.countDown()
    assertEquals(Await.result(first, 5.seconds), "A.scala")
    assertEquals(Await.result(second, 5.seconds), "B.scala")
  }

  test("unmapped misses share a serialized fallback lane") {
    val cache = new InteractiveSemanticdbCache[String, String]()
    val compileEntered = new CountDownLatch(2)
    val releaseCompile = new CountDownLatch(1)
    val activeCompilations = new AtomicInteger()
    val maximumActiveCompilations = new AtomicInteger()

    def lookup(path: String) = Future {
      cache.compute(
        path,
        InteractiveSemanticdbCompilationLane.Unmapped,
        _ => false,
      ) { _ =>
        val active = activeCompilations.incrementAndGet()
        maximumActiveCompilations.accumulateAndGet(active, Math.max)
        compileEntered.countDown()
        releaseCompile.await()
        activeCompilations.decrementAndGet()
        path
      }
    }

    val first = lookup("A.scala")
    val second = lookup("B.scala")
    assert(!compileEntered.await(250, TimeUnit.MILLISECONDS))
    assertEquals(maximumActiveCompilations.get(), 1)

    releaseCompile.countDown()
    assertEquals(Await.result(first, 5.seconds), "A.scala")
    assertEquals(Await.result(second, 5.seconds), "B.scala")
    assertEquals(maximumActiveCompilations.get(), 1)
  }
}
