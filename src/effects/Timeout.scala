package effect.effects

import caps.SharedCapability
import java.time.{Duration, Instant}

/** Timeout effect — deadline-based computation boundaries.
  *
  * Provides a capability to check remaining time and abort if a deadline has passed. Unlike Raise,
  * timeout is cooperative: the program checks `checkTimeout()` at appropriate points.
  */
trait Timeout extends SharedCapability:
  def deadline: Instant
  def remaining: Duration
  def checkTimeout(): Unit

object Timeout:

  inline def deadline(using t: Timeout): Instant = t.deadline
  inline def remaining(using t: Timeout): Duration = t.remaining
  inline def checkTimeout()(using t: Timeout): Unit = t.checkTimeout()

  /** Returns true if the deadline has passed. */
  inline def isExpired(using t: Timeout): Boolean = t.remaining.isNegative

  final class TimeoutException(val deadline: Instant)
      extends Exception(s"Deadline exceeded: $deadline", null, true, false)

  def handler[A](duration: Duration)(program: Timeout ?=> A): Option[A] =
    val dl = Instant.now().plus(duration)
    val cap = new Timeout:
      def deadline: Instant = dl
      def remaining: Duration = Duration.between(Instant.now(), dl)
      def checkTimeout(): Unit =
        if Instant.now().isAfter(dl) then throw new TimeoutException(dl)
    try Some(program(using cap))
    catch case _: TimeoutException => None

  def handlerOrThrow[A](duration: Duration)(program: Timeout ?=> A): A =
    val dl = Instant.now().plus(duration)
    val cap = new Timeout:
      def deadline: Instant = dl
      def remaining: Duration = Duration.between(Instant.now(), dl)
      def checkTimeout(): Unit =
        if Instant.now().isAfter(dl) then throw new TimeoutException(dl)
    program(using cap)

  /** Handler that returns Either[Duration, A] where Left contains elapsed time on timeout. */
  def handlerEither[A](duration: Duration)(program: Timeout ?=> A): Either[Duration, A] =
    val start = Instant.now()
    val dl = start.plus(duration)
    val cap = new Timeout:
      def deadline: Instant = dl
      def remaining: Duration = Duration.between(Instant.now(), dl)
      def checkTimeout(): Unit =
        if Instant.now().isAfter(dl) then throw new TimeoutException(dl)
    try Right(program(using cap))
    catch case _: TimeoutException => Left(Duration.between(start, Instant.now()))
