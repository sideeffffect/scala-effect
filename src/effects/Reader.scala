package effect.effects

import effect.core.Capability

/** Reader effect — corresponds to Effective's Ask/Local effects.
  *
  * In Effective: type Ask r = Alg (Ask_ r) type Local r = Scp (Local_ r) ask :: Member (Ask r) sig =>
  * Prog sig r local :: Member (Local r) sig => (r -> r) -> Prog sig a -> Prog sig a
  *
  * Ask is algebraic (simple read), Local is scoped (modifies env for a region). In Scala, both are
  * methods on the same capability trait. The scoped nature of `local` is naturally expressed as a
  * higher-order method taking a by-name block.
  */
trait Reader[R] extends Capability:
  def ask: R
  def local[A](f: R => R)(program: Reader[R] ?=> A): A

object Reader:

  /** Primitive operation: read the environment. */
  def ask[R](using r: Reader[R]): R = r.ask

  /** Primitive operation: project from the environment. */
  def asks[R, A](f: R => A)(using r: Reader[R]): A = f(r.ask)

  /** Primitive operation: run a computation with a modified environment.
    *
    * This is a scoped operation — it modifies the capability for a region. In Effective, this is a
    * Scp (scoped) effect requiring explicit forwarding through monad transformers. In Scala, it's
    * just a method call.
    */
  def local[R, A](f: R => R)(program: Reader[R] ?=> A)(using r: Reader[R]): A =
    r.local(f)(program)

  /** Handler: run a computation with a fixed environment.
    *
    * Corresponds to Effective's: reader :: r -> Handler '[Ask r, Local r] '[] '[ReaderT r] a a
    */
  def handler[R, A](env: R)(program: Reader[R] ?=> A): A =
    given Reader[R]:
      def ask: R = env
      def local[B](f: R => R)(prog: Reader[R] ?=> B): B =
        Reader.handler(f(env))(prog)
    program
