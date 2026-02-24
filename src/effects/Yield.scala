package effect.effects

import caps.SharedCapability

/** Yield/Emit effect — corresponds to Effective's Yield effect (coroutine-style).
  *
  * In Effective: type Yield a b = Alg (Yield_ a b) yield :: Member (Yield a b) sig => a -> Prog sig
  * b
  *
  * Yield produces a value and optionally receives a response. This enables generator/coroutine
  * patterns.
  */
trait Emit[A] extends SharedCapability:
  def emit(value: A): Unit

object Emit:

  def emit[A](value: A)(using e: Emit[A]): Unit = e.emit(value)

  /** Handler: collect all emitted values into a list.
    *
    * Corresponds to running a generator and collecting its output, similar to Effective's yield
    * handler that collects to a list.
    */
  def toList[A, B](program: Emit[A] ?=> B): (List[A], B) =
    val buffer = collection.mutable.ListBuffer.empty[A]
    val cap = new Emit[A]:
      def emit(value: A): Unit = buffer += value
    val result = program(using cap)
    (buffer.toList, result)

  /** Handler: fold over emitted values with an accumulator.
    *
    * Note: A direct implementation (capturing `f` in anonymous Emit) would violate the
    * SharedCapability self-type restriction — external references aren't in `{cap}`. So we delegate
    * to toList and fold afterward.
    */
  def fold[A, S, B](initial: S)(f: (S, A) => S)(program: Emit[A] ?=> B): (S, B) =
    val (values, result) = toList(program)
    (values.foldLeft(initial)(f), result)

  /** Scoped handler: transform emitted values before passing to outer handler.
    *
    * In Effective: type MapYield a b = Scp (MapYield_ a b) mapYield :: (a -> a) -> (b -> b) -> Prog
    * sig x -> Prog sig x
    *
    * Collects inner emissions, maps them, and re-emits. A direct forwarding approach would violate
    * SharedCapability's self-type restriction (can't capture `f` inside the anonymous class).
    */
  def mapEmit[A, B](f: A => A)(program: Emit[A] ?=> B)(using outer: Emit[A]): B =
    val (innerValues, result) = toList(program)
    innerValues.map(f).foreach(outer.emit)
    result
