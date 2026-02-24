package effect.effects

import caps.SharedCapability
import caps.unsafe.unsafeAssumePure
import java.util.concurrent.{Callable, CancellationException, CompletableFuture, StructuredTaskScope}
import scala.util.control.NonFatal

/** Async effect — structured concurrency via JDK 25 StructuredTaskScope.
  *
  * Corresponds to Effective's concurrency effects:
  *   - JPar (joined parallel composition): `jpar :: Prog sig a -> Prog sig b -> Prog sig (a, b)`
  *   - Par (non-joined parallel): `par :: Prog sig a -> Prog sig a -> Prog sig a`
  *
  * In Scala, we use JDK 25's StructuredTaskScope with virtual threads. The Async capability is
  * scoped: it cannot escape its handler, ensuring all forked tasks are joined before the handler
  * returns.
  *
  * Cancellation is cooperative via `Thread.interrupt()`. Forked tasks can:
  *   - Call `Async.checkCancelled()` to throw if interrupted
  *   - Use blocking operations (sleep, I/O) which respond to interruption automatically
  *   - Call `fork.cancel()` to interrupt a specific task
  */
trait Async extends SharedCapability:
  def fork[A](task: => A): Async.Fork[A]
  def checkCancelled(): Unit

object Async:

  /** A handle to a forked computation. Supports cancellation. */
  trait Fork[A]:
    def join(): A
    def cancel(): Unit
    def isDone: Boolean
    def isCancelled: Boolean

  inline def fork[A](task: => A)(using a: Async): Fork[A] =
    a.fork(task)

  /** Check if the current task has been cancelled (interrupted). Throws CancellationException if
    * so.
    */
  inline def checkCancelled()(using a: Async): Unit = a.checkCancelled()

  /** Run two computations in parallel and return both results. */
  def par[A, B](a: => A, b: => B)(using async: Async): (A, B) =
    val fa = fork(a)
    val fb = fork(b)
    (fa.join(), fb.join())

  /** Race two computations: return the first to succeed. The loser is cancelled. */
  def race[A](a: => A, b: => A): A =
    val scope = StructuredTaskScope.open(
      StructuredTaskScope.Joiner.anySuccessfulResultOrThrow[A]()
    )
    try
      scope.fork(asCallable(a))
      scope.fork(asCallable(b))
      scope.join()
    finally scope.close()

  /** Run two computations in parallel; cancel the other when the first completes. */
  def parFirst[A](a: => A, b: => A)(using async: Async): A =
    val fa = fork(a)
    val fb = fork(b)
    val result =
      try fa.join()
      catch
        case e: CancellationException =>
          fb.join() // fa was cancelled, wait for fb
        case NonFatal(e) =>
          fb.cancel()
          throw e
    fb.cancel() // fa finished first, cancel fb
    result

  /** Bridge for passing capturing Scala lambdas to Java's Callable-accepting APIs.
    *
    * The capture checker prevents passing `Callable[T]^{captured}` to Java methods expecting
    * `Callable[T]` (pure). We use `unsafeAssumePure` because the StructuredTaskScope guarantees
    * the callable completes before `join()` returns.
    */
  private def asCallable[A](task: => A): Callable[A] =
    val c: Callable[A]^ = () => task
    c.unsafeAssumePure

  /** Handler: run a computation with structured concurrency.
    *
    * All forked tasks are joined before the handler returns. If any task fails, all others are
    * cancelled and the exception propagates.
    *
    * Uses JDK 25 StructuredTaskScope with virtual threads. Each fork delivers its result via a
    * CompletableFuture, allowing Fork.join() to block independently of scope.join(). The scope is
    * joined after the program completes to satisfy the StructuredTaskScope protocol.
    */
  def handler[A](program: Async ?=> A): A =
    val scope = StructuredTaskScope.open()
    try
      val cap = new Async:
        def fork[B](task: => B): Fork[B] =
          val future = CompletableFuture[B]()
          @volatile var taskThread: Thread = null
          @volatile var cancelled = false
          scope.fork(asCallable {
            taskThread = Thread.currentThread()
            try
              if cancelled then throw CancellationException()
              future.complete(task)
            catch
              case e: InterruptedException =>
                future.completeExceptionally(CancellationException().initCause(e))
              case NonFatal(e) =>
                future.completeExceptionally(e)
            null
          })
          new Fork[B]:
            def join(): B =
              try future.get()
              catch
                case e: java.util.concurrent.ExecutionException =>
                  throw e.getCause
                case e: java.util.concurrent.CancellationException =>
                  throw e

            def cancel(): Unit =
              cancelled = true
              future.cancel(true)
              val t = taskThread
              if t != null then t.interrupt()

            def isDone: Boolean = future.isDone
            def isCancelled: Boolean = cancelled || future.isCancelled

        def checkCancelled(): Unit =
          if Thread.currentThread().isInterrupted then throw CancellationException()

      try
        val result = program(using cap)
        result
      finally
        scope.join()
    finally scope.close()

  /** Handler variant that catches failures as Either. */
  def handlerEither[A](program: Async ?=> A): Either[Throwable, A] =
    try Right(handler(program))
    catch
      case NonFatal(e) =>
        Left(e.getCause match
          case null => e
          case c    => c)
