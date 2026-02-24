package effect.effects

import caps.SharedCapability

/** Reader effect — corresponds to Effective's Ask/Local effects.
  *
  * Ask is algebraic (simple read), Local is scoped (modifies env for a region). In Scala, both are
  * methods on the same capability trait. The scoped nature of `local` is naturally expressed as a
  * higher-order method taking a by-name block.
  */
trait Reader[R] extends SharedCapability:
  def ask: R
  def local[A](f: R => R)(program: Reader[R] ?=> A): A

object Reader:

  inline def ask[R](using r: Reader[R]): R = r.ask

  inline def asks[R, A](f: R => A)(using r: Reader[R]): A = f(r.ask)

  /** Scoped operation: run a computation with a modified environment. */
  inline def local[R, A](f: R => R)(program: Reader[R] ?=> A)(using r: Reader[R]): A =
    r.local(f)(program)

  def handler[R, A](env: R)(program: Reader[R] ?=> A): A =
    given Reader[R]:
      def ask: R = env
      def local[B](f: R => R)(prog: Reader[R] ?=> B): B =
        Reader.handler(f(env))(prog)
    program
