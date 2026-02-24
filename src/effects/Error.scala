package effect.effects

import caps.SharedCapability

/** Error effect — corresponds to Effective's Throw/Catch effects.
  *
  * In Effective: type Throw e = Alg (Throw_ e) type Catch e = Scp (Catch_ e) throw :: Member (Throw
  * e) sig => e -> Prog sig a catch :: Member (Catch e) sig => Prog sig a -> (e -> Prog sig a) ->
  * Prog sig a
  *
  * Throw is algebraic (abandon computation), Catch is scoped (delimit a region).
  *
  * Implementation uses plain exceptions rather than boundary/break because Scala 3.8's capture
  * checker tracks boundary labels as capabilities, making anonymous Raise instances capture the
  * label: `Raise[E]^{label}`. Exceptions avoid this capture issue while maintaining identical
  * semantics (boundary/break compiles to exceptions anyway).
  */
trait Raise[E] extends SharedCapability:
  def raise(error: E): Nothing

object Raise:

  /** Internal exception used to implement error short-circuiting. */
  final private class RaiseException[E](val error: E) extends Exception(null, null, true, false)

  /** Primitive operation: raise an error, aborting the current computation. */
  def raise[E](error: E)(using r: Raise[E]): Nothing = r.raise(error)

  /** Handler: run a computation that may fail, returning Either.
    *
    * Corresponds to Effective's: except :: Handler '[Throw e, Catch e] '[] '[ExceptT e] a (Either e
    * a)
    */
  def handler[E, A](program: Raise[E] ?=> A): Either[E, A] =
    val token = new AnyRef // unique token to identify this handler
    val cap = new Raise[E]:
      def raise(error: E): Nothing =
        throw new RaiseException((token, error))
    try Right(program(using cap))
    catch
      case ex: RaiseException[?] if ex.error.asInstanceOf[(AnyRef, ?)]._1 eq token =>
        Left(ex.error.asInstanceOf[(AnyRef, E)]._2)

  /** Handler: run a computation that may fail, returning Option.
    *
    * Corresponds to Effective's Maybe-based except: except :: Handler '[Throw, Catch] '[] '[MaybeT]
    * a (Maybe a)
    */
  def toOption[E, A](program: Raise[E] ?=> A): Option[A] =
    handler(program).toOption

  /** Catch an error and recover with a handler function.
    *
    * Corresponds to Effective's scoped Catch operation: catch :: Member (Catch e) sig => Prog sig a
    * -> (e -> Prog sig a) -> Prog sig a
    */
  def catchError[E, A](program: Raise[E] ?=> A)(recover: E => A): A =
    handler(program) match
      case Right(a) => a
      case Left(e)  => recover(e)

  /** Retry a computation, using a recovery that can also fail. */
  def retry[E, A](program: Raise[E] ?=> A)(recover: E => Raise[E] ?=> A)(using
      r: Raise[E]
  ): A =
    handler(program) match
      case Right(a) => a
      case Left(e)  => recover(e)

/** Convenience: an error effect for simple failure without an error value.
  *
  * Corresponds to Effective's: type Throw = Alg Throw_ data Throw_ k = Throw
  */
trait Fail extends SharedCapability:
  def fail(): Nothing

object Fail:
  final private class FailException(val token: AnyRef) extends Exception(null, null, true, false)

  def fail()(using f: Fail): Nothing = f.fail()

  def handler[A](program: Fail ?=> A): Option[A] =
    val token = new AnyRef
    val cap = new Fail:
      def fail(): Nothing = throw new FailException(token)
    try Some(program(using cap))
    catch case ex: FailException if ex.token eq token => None
