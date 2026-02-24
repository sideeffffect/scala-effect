package effect.effects

import caps.SharedCapability

/** Reader effect — corresponds to Effective's Ask/Local effects.
  *
  * In Effective: type Ask r = Alg (Ask_ r) type Local r = Scp (Local_ r) ask :: Member (Ask r) sig
  * => Prog sig r local :: Member (Local r) sig => (r -> r) -> Prog sig a -> Prog sig a
  *
  * Ask is algebraic (simple read), Local is scoped (modifies env for a region). In Scala, both are
  * methods on the same capability trait. The scoped nature of `local` is naturally expressed as a
  * higher-order method taking a by-name block.
  */
trait Reader[R] extends SharedCapability:
  def ask: R
  def local[A](f: R => R)(program: Reader[R] ?=> A): A

object Reader:

  inline def ask[R](using Reader[R]): R = summon[Reader[R]].ask

  inline def asks[R, A](f: R => A)(using Reader[R]): A = f(summon[Reader[R]].ask)

  /** Scoped operation: run a computation with a modified environment. */
  inline def local[R, A](f: R => R)(program: Reader[R] ?=> A)(using Reader[R]): A =
    summon[Reader[R]].local(f)(program)

  def handler[R, A](env: R)(program: Reader[R] ?=> A): A =
    given Reader[R]:
      def ask: R = env
      def local[B](f: R => R)(prog: Reader[R] ?=> B): B =
        Reader.handler(f(env))(prog)
    program
