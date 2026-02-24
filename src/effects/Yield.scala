package effect.effects

import caps.SharedCapability

/** Yield/Emit effect — corresponds to Effective's Yield effect (coroutine-style).
  *
  * Yield produces a value and optionally receives a response. This enables generator/coroutine
  * patterns.
  */
trait Emit[A] extends SharedCapability:
  def emit(value: A): Unit

object Emit:

  inline def emit[A](value: A)(using e: Emit[A]): Unit = e.emit(value)

  /** Handler: collect all emitted values into a list.
    *
    * Returns a named tuple of (values, result).
    */
  def toList[A, B](program: Emit[A] ?=> B): (values: List[A], result: B) =
    val buffer = collection.mutable.ListBuffer.empty[A]
    val cap = new Emit[A]:
      def emit(value: A): Unit = buffer += value
    val result = program(using cap)
    (values = buffer.toList, result = result)

  /** Handler: fold over emitted values with an accumulator.
    *
    * Note: A direct implementation (capturing `f` in anonymous Emit) would violate the
    * SharedCapability self-type restriction. So we delegate to toList and fold afterward.
    */
  def fold[A, S, B](initial: S)(f: (S, A) => S)(
      program: Emit[A] ?=> B
  ): (accumulated: S, result: B) =
    val (values, result) = toList(program)
    (accumulated = values.foldLeft(initial)(f), result = result)

  /** Scoped handler: transform emitted values before passing to outer handler.
    *
    * Collects inner emissions, maps them, and re-emits. A direct forwarding approach would violate
    * SharedCapability's self-type restriction (can't capture `f` inside the anonymous class).
    */
  def mapEmit[A, B](f: A => A)(program: Emit[A] ?=> B)(using outer: Emit[A]): B =
    val (innerValues, result) = toList(program)
    innerValues.map(f).foreach(outer.emit)
    result
