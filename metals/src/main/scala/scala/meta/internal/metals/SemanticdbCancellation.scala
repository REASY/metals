package scala.meta.internal.metals

import java.util.concurrent.CancellationException

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise

import scala.meta.pc.CancelToken

/** Cancel both the BSP request and our wait even if the server never responds. */
private[metals] object SemanticdbCancellation {
  def during[A](token: CancelToken)(
      operation: Promise[Unit] => Future[A]
  )(implicit ec: ExecutionContext): Future[A] = {
    val cancelClasspath = Promise[Unit]()
    val result = Promise[A]()
    token.onCancel.whenComplete { (cancelled, _) =>
      if (java.lang.Boolean.TRUE == cancelled) {
        cancelClasspath.trySuccess(())
        result.tryFailure(
          new CancellationException("SemanticDB request cancelled")
        )
      }
    }
    if (!result.isCompleted) {
      token.checkCanceled()
      operation(cancelClasspath).onComplete(result.tryComplete)
    }
    result.future
  }
}
