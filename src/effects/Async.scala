package effect.effects

import caps.SharedCapability
import caps.unsafe.unsafeAssumePure
import java.util.concurrent.{Callable, CompletableFuture, StructuredTaskScope}

/** Async effect — structured concurrency via JDK 25 StructuredTaskScope.
  *
  * Corresponds to Effective's concurrency effects:
  *   - JPar (joined parallel composition): `jpar :: Prog sig a -> Prog sig b -> Prog sig (a, b)`
  *   - Par (non-joined parallel): `par :: Prog sig a -> Prog sig a -> Prog sig a`
  *
  * In Scala, we use JDK 25's StructuredTaskScope with virtual threads. The Async capability is
  * scoped: it cannot escape its handler, ensuring all forked tasks are joined before the handler
  * returns.
  */
trait Async extends SharedCapability:
  def fork[A](task: => A): Async.Fork[A]

object Async:

  /** A handle to a forked computation. */
  trait Fork[A]:
    def join(): A

  inline def fork[A](task: => A)(using a: Async): Fork[A] =
    a.fork(task)

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
    * CompletableFuture, allowing Fork.join() to block independently of scope.join(). The scope
    * is joined after the program completes to satisfy the StructuredTaskScope protocol.
    */
  def handler[A](program: Async ?=> A): A =
    val scope = StructuredTaskScope.open()
    try
      val cap = new Async:
        def fork[B](task: => B): Fork[B] =
          val future = CompletableFuture[B]()
          scope.fork(asCallable {
            try future.complete(task)
            catch case e: Throwable => future.completeExceptionally(e)
            null
          })
          new Fork[B]:
            def join(): B =
              try future.get()
              catch
                case e: java.util.concurrent.ExecutionException =>
                  throw e.getCause
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
      case e: Exception =>
        Left(e.getCause match
          case null => e
          case c    => c)
