package scala.meta.internal.metals

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.Future

private[metals] final class OpenedFileSemanticReadiness[K] {
  private val readinessByPath = new ConcurrentHashMap[K, Future[Unit]]()

  def track(
      path: K,
      readiness: Future[Unit],
  ): Future[Unit] = {
    readinessByPath.put(path, readiness)
    readiness
  }

  def await(path: K): Future[Unit] =
    Option(readinessByPath.get(path)).getOrElse(Future.unit)

  def remove(path: K): Unit = {
    readinessByPath.remove(path)
    ()
  }
}
