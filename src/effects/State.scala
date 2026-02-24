package effect.effects

import caps.SharedCapability

/** State effect — corresponds to Effective's Get/Put effects.
  *
  * In Effective: type Get s = Alg (Get_ s) type Put s = Alg (Put_ s) get :: Member (Get s) sig =>
  * Prog sig s put :: Member (Put s) sig => s -> Prog sig ()
  *
  * In Scala, we model this as a capability trait with get/set methods. Both Get and Put are unified
  * into a single State capability, since Scala capabilities don't require the artificial split that
  * effect rows demand.
  */
trait State[S] extends SharedCapability:
  def get: S
  def set(s: S): Unit

object State:

  inline def get[S](using State[S]): S = summon[State[S]].get

  inline def set[S](value: S)(using State[S]): Unit = summon[State[S]].set(value)

  inline def modify[S](f: S => S)(using State[S]): Unit =
    val s = summon[State[S]]
    s.set(f(s.get))

  inline def gets[S, A](f: S => A)(using State[S]): A = f(summon[State[S]].get)

  /** Handler: run a stateful computation with mutable state.
    *
    * Returns a named tuple of (state, result).
    */
  def handler[S, A](initial: S)(program: State[S] ?=> A): (state: S, result: A) =
    var current: S = initial
    val cap = new State[S]:
      def get: S = current
      def set(s: S): Unit = current = s
    val result = program(using cap)
    (state = current, result = result)

  /** Handler variant that discards the final state. */
  def handler_[S, A](initial: S)(program: State[S] ?=> A): A =
    handler(initial)(program).result

  /** Handler that only returns the final state. */
  def execHandler[S, A](initial: S)(program: State[S] ?=> A): S =
    handler(initial)(program).state
