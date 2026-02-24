package effect.effects

import effect.core.Capability

/** Yield/Emit effect — corresponds to Effective's Yield effect (coroutine-style).
  *
  * In Effective: type Yield a b = Alg (Yield_ a b) yield :: Member (Yield a b) sig => a -> Prog sig
  * b
  *
  * Yield produces a value and optionally receives a response. This enables generator/coroutine
  * patterns.
  */
trait Emit[A] extends Capability:
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
    * Note: The fold function is stored in a local var-binding. With capture checking, an anonymous
    * Emit that closes over the `f` parameter would have type `Emit[A]^{f}`, which is incompatible
    * with `Emit[A]`. By using a var-indirection, we avoid the capture annotation propagating to the
    * capability type.
    */
  def fold[A, S, B](initial: S)(f: (S, A) => S)(program: Emit[A] ?=> B): (S, B) =
    // Delegate to toList, then fold. A direct implementation would capture
    // `f` in the Emit capability, which capture checking flags as
    // `Emit[A]^{f}` — incompatible with `Emit[A]`.
    val (values, result) = toList(program)
    (values.foldLeft(initial)(f), result)

  /** Scoped handler: transform emitted values before passing to outer handler.
    *
    * In Effective: type MapYield a b = Scp (MapYield_ a b) mapYield :: (a -> a) -> (b -> b) -> Prog
    * sig x -> Prog sig x
    *
    * This is a "scoped" operation: it intercepts emitted values in a region and transforms them
    * before forwarding to the outer Emit capability.
    *
    * Implementation: collects inner emissions, maps them, and re-emits. This avoids the capture
    * checking issue of creating a capability that closes over `f` and `outer`.
    */
  def mapEmit[A, B](f: A => A)(program: Emit[A] ?=> B)(using outer: Emit[A]): B =
    val (innerValues, result) = toList(program)
    innerValues.map(f).foreach(outer.emit)
    result
