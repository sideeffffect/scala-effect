package effect.effects

import caps.SharedCapability

/** Error effect — corresponds to Effective's Throw/Catch effects.
  *
  * Throw is algebraic (abandon computation), Catch is scoped (delimit a region).
  *
  * Implementation uses plain exceptions rather than boundary/break because Scala 3.8's capture
  * checker tracks boundary labels as capabilities, making anonymous Raise instances capture the
  * label: `Raise[E]^{label}`. Exceptions avoid this capture issue while maintaining identical
  * semantics (boundary/break compiles to exceptions anyway).
  */
trait Raise[E <: Exception] extends SharedCapability:
  def raise(error: E): Nothing

object Raise:

  final private class RaiseException[E <: Exception](val token: AnyRef, val error: E)
      extends Exception(null, error, true, false)

  inline def raise[E <: Exception](error: E)(using r: Raise[E]): Nothing =
    r.raise(error)

  def handler[E <: Exception, A](program: Raise[E] ?=> A): Either[E, A] =
    val token = new AnyRef
    val cap = new Raise[E]:
      def raise(error: E): Nothing =
        throw new RaiseException(token, error)
    try Right(program(using cap))
    catch
      case ex: RaiseException[?] if ex.token eq token =>
        Left(ex.error.asInstanceOf[E])

  def toOption[E <: Exception, A](program: Raise[E] ?=> A): Option[A] =
    handler(program).toOption

  def catchError[E <: Exception, A](program: Raise[E] ?=> A)(recover: E => A): A =
    handler(program) match
      case Right(a) => a
      case Left(e)  => recover(e)

  def retry[E <: Exception, A](program: Raise[E] ?=> A)(recover: E => Raise[E] ?=> A)(using
      r: Raise[E]
  ): A =
    handler(program) match
      case Right(a) => a
      case Left(e)  => recover(e)

/** Convenience: an error effect for simple failure without an error value. */
trait Fail extends SharedCapability:
  def fail(): Nothing

object Fail:
  final private class FailException(val token: AnyRef) extends Exception(null, null, true, false)

  inline def fail()(using f: Fail): Nothing = f.fail()

  def handler[A](program: Fail ?=> A): Option[A] =
    val token = new AnyRef
    val cap = new Fail:
      def fail(): Nothing = throw new FailException(token)
    try Some(program(using cap))
    catch case ex: FailException if ex.token eq token => None
