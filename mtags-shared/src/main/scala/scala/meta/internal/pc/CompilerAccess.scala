package scala.meta.internal.pc

import java.time.Duration
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.ExecutionContextExecutor
import scala.util.control.NonFatal

import scala.meta.internal.metals.PcQueryContext
import scala.meta.pc.CancelToken
import scala.meta.pc.PresentationCompilerConfig

import org.slf4j.Logger

/**
 * Manages the lifecycle and multi-threaded access to the presentation compiler.
 *
 * - automatically restarts the compiler on miscellaneous crashes.
 * - handles cancellation via `Thread.interrupt()` to stop the compiler during typechecking,
 *   for functions that support cancellation.
 */
abstract class CompilerAccess[Reporter, Compiler](
    logger: Logger,
    config: PresentationCompilerConfig,
    sh: Option[ScheduledExecutorService],
    newCompiler: () => CompilerWrapper[Reporter, Compiler],
    shouldResetJobQueue: Boolean,
    id: String = ""
)(implicit ec: ExecutionContextExecutor) {

  private val jobs = CompilerJobQueue(id)
  // A canceled future does not prove that a macro or compiler thread stopped.
  // Never queue more work or create another generation after a hard timeout.
  private val quarantined = new AtomicBoolean(false)
  private val compilerGeneration = new AtomicLong(0L)
  private val compilerInvalidations = new AtomicLong(0L)
  def contextEpoch: Long = compilerInvalidations.get()
  def isQuarantined: Boolean = quarantined.get()
  def generation: Long = compilerGeneration.get()
  // All job state and compiler ownership transitions use this instance's lock.
  private sealed trait JobState
  private case object Queued extends JobState
  private case object Running extends JobState
  private case object Finished extends JobState
  private case object TimedOut extends JobState
  private final class CompilerJob {
    var state: JobState = Queued
    var thread: Thread = _
    var compiler: CompilerWrapper[Reporter, Compiler] = _
    var generation: Long = -1L
  }
  private var activeJob: CompilerJob = _
  private var _compiler: CompilerWrapper[Reporter, Compiler] = _
  private var isShutdown: Boolean = false
  private def isEmpty: Boolean = _compiler == null

  private def loadCompiler(): CompilerWrapper[Reporter, Compiler] =
    synchronized {
      val job = activeJob
      val ownsJob = job != null && (job.thread eq Thread.currentThread())
      if (quarantined.get())
        throw new CompilerAccess.CompilerQuarantinedException
      if (isShutdown || (ownsJob && job.state != Running)) {
        throw new CancellationException()
      }
      if (_compiler == null) {
        _compiler = newCompiler()
        compilerGeneration.incrementAndGet()
      }
      if (ownsJob) {
        job.compiler = _compiler
        job.generation = compilerGeneration.get()
      }
      _compiler.resetReporter()
      _compiler
    }

  protected def newReporter: Reporter

  def reporter: Reporter =
    if (isEmpty) newReporter
    else _compiler.reporterAccess.reporter

  def isLoaded(): Boolean = _compiler != null

  def shutdown(): Unit = {
    val compiler = synchronized {
      isShutdown = true
      detachCurrentCompiler()
    }
    stopCompiler(compiler)
    jobs.shutdown()
  }

  def shutdownCurrentCompiler(): Unit = {
    // Detach under the same lock as loadCompiler so a concurrent load cannot
    // obtain the compiler being stopped or be overwritten afterwards.
    val compiler = synchronized {
      detachCurrentCompiler()
    }
    stopCompiler(compiler)
  }

  private def detachCurrentCompiler(): CompilerWrapper[Reporter, Compiler] = {
    val current = _compiler
    _compiler = null
    compilerGeneration.incrementAndGet()
    compilerInvalidations.incrementAndGet()
    current
  }

  private def stopCompiler(
      compiler: CompilerWrapper[Reporter, Compiler]
  ): Unit = {
    if (compiler != null) {
      compiler.stop()
      sh.foreach { scheduler =>
        scheduler.schedule[Unit](
          () => {
            if (compiler.isAlive()) {
              compiler.stop()
            }
          },
          2,
          TimeUnit.SECONDS
        )
      }
    }
  }

  /**
   * Asynchronously execute a function on the compiler thread with `Thread.interrupt()` cancellation.
   */
  def withInterruptableCompiler[T](
      default: T,
      token: CancelToken,
      customTimeout: Option[Duration] = None
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): CompletableFuture[T] = {
    onCompilerJobQueue(
      () => withSharedCompiler(default)(thunk),
      token,
      customTimeout,
      interruptOnCancel = true
    )
  }

  /**
   * Asynchronously execute a function on the compiler thread without `Thread.interrupt()` cancellation.
   *
   * Note that the function is still cancellable.
   */
  def withNonInterruptableCompiler[T](
      default: T,
      token: CancelToken
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): CompletableFuture[T] = {
    onCompilerJobQueue(
      () => withSharedCompiler(default)(thunk),
      token
    )
  }

  /**
   * Execute a function on the current thread without cancellation support.
   *
   * May potentially run in parallel with other requests, use carefully.
   */
  def withSharedCompiler[T](
      default: T
  )(
      thunk: CompilerWrapper[Reporter, Compiler] => T
  )(implicit queryInfo: PcQueryContext): T = {
    try {
      val compiler =
        try Right(loadCompiler())
        catch { case _: CancellationException => Left(default) }
      compiler.map(thunk).getOrElse(default)
    } catch {
      case error: CompilerAccess.CompilerQuarantinedException => throw error
      case InterruptException() =>
        default
      case other: Throwable =>
        handleSharedCompilerException(other)
          .map { message =>
            retryWithCleanCompiler(
              thunk,
              default,
              message,
              other
            )
          }
          .getOrElse {
            handleError(other)
            default
          }
    }
  }

  // TODO: make this return `String`, we should not hide meaningful exceptions
  protected def handleSharedCompilerException(t: Throwable): Option[String]

  protected def ignoreException(t: Throwable): Boolean

  private def retryWithCleanCompiler[T](
      thunk: CompilerWrapper[Reporter, Compiler] => T,
      default: T,
      message: String,
      cause: Throwable
  )(implicit queryInfo: PcQueryContext): T = {
    shutdownCurrentCompiler()
    logger.warn(
      s"compiler crashed due to $message, retrying with new compiler instance.",
      cause
    )
    try thunk(loadCompiler())
    catch {
      case error: CompilerAccess.CompilerQuarantinedException => throw error
      case InterruptException() =>
        default
      case NonFatal(e) =>
        handleError(e)
        default
    }
  }

  private def handleError(
      e: Throwable
  )(implicit queryInfo: PcQueryContext): Unit = {
    queryInfo.report("compiler-error", e, "")
    shutdownCurrentCompiler()
  }

  private def onCompilerJobQueue[T](
      thunk: () => T,
      token: CancelToken,
      customTimeout: Option[Duration] = None,
      interruptOnCancel: Boolean = false
  ): CompletableFuture[T] = {
    val result = new CompletableFuture[T]()
    val job = new CompilerJob
    if (quarantined.get()) {
      result.completeExceptionally(
        new CompilerAccess.CompilerQuarantinedException
      )
      return result
    }
    def finishJob(): Boolean = synchronized {
      val publish = job.state == Running
      job.state = Finished
      if (activeJob eq job) activeJob = null
      publish
    }
    jobs.submit(
      result,
      { () =>
        val shouldRun = synchronized {
          if (
            job.state != Queued || result.isDone() ||
            token.isCanceled() || quarantined.get()
          ) {
            job.state = Finished
            false
          } else {
            job.state = Running
            job.thread = Thread.currentThread()
            activeJob = job
            true
          }
        }
        if (!shouldRun)
          result.completeExceptionally(
            if (quarantined.get())
              new CompilerAccess.CompilerQuarantinedException
            else new CancellationException()
          )
        else {
          try {
            Thread.interrupted() // clear interrupt bit
            val value = thunk()
            // Relinquish ownership atomically before publishing a completion
            // whose callbacks could allow another compiler job to start.
            if (finishJob()) result.complete(value)
          } catch {
            case NonFatal(e) =>
              logger.error(s"presentation compiler error ${e.getMessage()}", e)
              if (finishJob()) result.completeExceptionally(e)
          } finally finishJob()
        }
      }
    )

    // Interrupt only the exact running job's captured compiler, under the
    // lifecycle lock so completion cannot transfer ownership in between.
    token.onCancel.whenCompleteAsync(
      (isCancelled, _) => {
        if (isCancelled) {
          synchronized {
            if (
              interruptOnCancel && job.state == Running &&
              (activeJob eq job) && job.compiler != null
            ) {
              job.compiler.presentationCompilerThread.foreach { pcThread =>
                pcThread.interrupt()
                if (job.thread ne pcThread) job.thread.interrupt()
              }
            }
          }
          result.cancel(false)
        }
      },
      ec
    )
    // Bind the watchdog to this job's lifecycle and exact compiler generation.
    // CompilerJobQueue skips canceled futures without invoking our runnable, so
    // a never-started job must not claim ownership of the current compiler.
    sh.foreach { scheduler =>
      scheduler.schedule[Unit](
        { () =>
          var shouldCancel = false
          var compilerToStop: CompilerWrapper[Reporter, Compiler] = null
          synchronized {
            job.state match {
              case Queued =>
                job.state = Finished
                shouldCancel = true
              case Running if activeJob eq job =>
                job.state = TimedOut
                shouldCancel = true
                compilerToStop = job.compiler
                if (
                  job.compiler != null &&
                  (_compiler eq job.compiler) &&
                  compilerGeneration.get() == job.generation
                ) {
                  if (customTimeout.nonEmpty) quarantined.set(true)
                  compilerToStop = detachCurrentCompiler()
                  if (shouldResetJobQueue) jobs.reset()
                }
              case _ =>
            }
          }
          try {
            if (shouldCancel) result.cancel(false)
            // Stop only the captured wrapper, never a subsequently loaded one.
            stopCompiler(compilerToStop)
          } catch {
            case NonFatal(_) =>
            case other: Throwable =>
              if (!ignoreException(other)) throw other
          }
        },
        customTimeout.map(_.toMillis()).getOrElse(config.timeoutDelay()),
        customTimeout
          .map(_ => TimeUnit.MILLISECONDS)
          .getOrElse(config.timeoutUnit())
      )
    }

    result
  }
}

object CompilerAccess {
  final class CompilerQuarantinedException
      extends IllegalStateException("CODEHOP_COMPILER_QUARANTINED")
}
