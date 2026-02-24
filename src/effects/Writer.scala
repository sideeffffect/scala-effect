package effect.effects

import effect.core.Capability

/** Writer effect — corresponds to Effective's Tell/Censor effects.
  *
  * In Effective: type Tell w = Alg (Tell_ w) type Censor w = Scp (Censor_ w) tell :: Monoid w =>
  * Member (Tell w) sig => w -> Prog sig () censor :: Member (Censor w) sig => (w -> w) -> Prog sig
  * a -> Prog sig a
  *
  * Tell is algebraic (append to log), Censor is scoped (transform log for region).
  */
trait Writer[W] extends Capability:
  def tell(w: W): Unit
  def censor[A](f: W => W)(program: Writer[W] ?=> A): A

object Writer:

  /** Primitive operation: append to the log. */
  def tell[W](w: W)(using wr: Writer[W]): Unit = wr.tell(w)

  /** Primitive operation: transform the output of a region.
    *
    * This is a scoped operation. In Effective, censor requires a dedicated handler (censors) that
    * must be composed with the writer handler: handle (censors id |> writer) hoppy
    *
    * In Scala, it's just a method on the capability.
    */
  def censor[W, A](f: W => W)(program: Writer[W] ?=> A)(using wr: Writer[W]): A =
    wr.censor(f)(program)

  /** Handler: run a computation collecting output into a List.
    *
    * Corresponds to Effective's: writer :: Monoid w => Handler '[Tell w] '[] '[WriterT w] a (w, a)
    *
    * Returns (collected output, result).
    */
  def handler[W, A](program: Writer[W] ?=> A): (List[W], A) =
    val buffer = collection.mutable.ListBuffer.empty[W]
    val cap = new Writer[W]:
      def tell(w: W): Unit = buffer += w
      def censor[B](f: W => W)(prog: Writer[W] ?=> B): B =
        val (inner, result) = Writer.handler(prog)
        inner.map(f).foreach(tell)
        result
    val result = program(using cap)
    (buffer.toList, result)

  /** Handler variant that discards the output. */
  def handler_[W, A](program: Writer[W] ?=> A): A =
    handler(program)._2

  /** Handler: run collecting output with a custom combine.
    *
    * Uses a Monoid-like combine for accumulation instead of List.
    *
    * Note: Unlike `handler`, this version does not support `censor` because the anonymous
    * capability would capture the `combine` function, which capture checking correctly flags as a
    * potential escape. Use the List-based `handler` for censor support.
    */
  def handlerWith[W, A](empty: W)(combine: (W, W) => W)(program: Writer[W] ?=> A): (W, A) =
    // Delegate to the list-based handler, then fold the results.
    // A direct implementation would create a capability that captures
    // `combine`, which capture checking correctly flags as `Writer[W]^{combine}`.
    // This indirect approach avoids the capture issue entirely.
    val (values, result) = handler(program)
    (values.foldLeft(empty)(combine), result)
