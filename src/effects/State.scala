package effect.effects

import effect.core.Capability

/** State effect — corresponds to Effective's Get/Put effects.
  *
  * In Effective: type Get s = Alg (Get_ s) type Put s = Alg (Put_ s) get :: Member (Get s) sig =>
  * Prog sig s put :: Member (Put s) sig => s -> Prog sig ()
  *
  * In Scala, we model this as a capability trait with get/set methods. Both Get and Put are unified
  * into a single State capability, since Scala capabilities don't require the artificial split that
  * effect rows demand.
  */
trait State[S] extends Capability:
  def get: S
  def set(s: S): Unit

object State:

  /** Primitive operation: read the current state. */
  def get[S](using s: State[S]): S = s.get

  /** Primitive operation: write a new state. */
  def set[S](value: S)(using s: State[S]): Unit = s.set(value)

  /** Primitive operation: modify the state with a function. */
  def modify[S](f: S => S)(using s: State[S]): Unit =
    s.set(f(s.get))

  /** Primitive operation: get the state and apply a projection. */
  def gets[S, A](f: S => A)(using s: State[S]): A = f(s.get)

  /** Handler: run a stateful computation with mutable state.
    *
    * Corresponds to Effective's: state :: s -> Handler '[Put s, Get s] '[] '[StateT s] a (s, a)
    *
    * Returns a tuple of (final state, result).
    */
  def handler[S, A](initial: S)(program: State[S] ?=> A): (S, A) =
    var current: S = initial
    val cap = new State[S]:
      def get: S = current
      def set(s: S): Unit = current = s
    val result = program(using cap)
    (current, result)

  /** Handler variant that discards the final state.
    *
    * Corresponds to Effective's: state_ :: s -> Handler '[Put s, Get s] '[] '[StateT s] a a
    */
  def handler_[S, A](initial: S)(program: State[S] ?=> A): A =
    handler(initial)(program)._2

  /** Handler that only returns the final state. */
  def execHandler[S, A](initial: S)(program: State[S] ?=> A): S =
    handler(initial)(program)._1
