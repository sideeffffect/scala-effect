package effect.effects

import caps.SharedCapability

/** Writer effect — corresponds to Effective's Tell/Censor effects.
  *
  * Tell is algebraic (append to log), Censor is scoped (transform log for region).
  */
trait Writer[W] extends SharedCapability:
  def tell(w: W): Unit
  def censor[A](f: W => W)(program: Writer[W] ?=> A): A

object Writer:

  inline def tell[W](w: W)(using wr: Writer[W]): Unit = wr.tell(w)

  /** Scoped operation: transform the output of a region. */
  inline def censor[W, A](f: W => W)(program: Writer[W] ?=> A)(using wr: Writer[W]): A =
    wr.censor(f)(program)

  /** Handler: run a computation collecting output into a List.
    *
    * Returns a named tuple of (output, result).
    */
  def handler[W, A](program: Writer[W] ?=> A): (output: List[W], result: A) =
    val buffer = collection.mutable.ListBuffer.empty[W]
    val cap = new Writer[W]:
      def tell(w: W): Unit = buffer += w
      def censor[B](f: W => W)(prog: Writer[W] ?=> B): B =
        val (inner, result) = Writer.handler(prog)
        inner.map(f).foreach(tell)
        result
    val result = program(using cap)
    (output = buffer.toList, result = result)

  /** Handler variant that discards the output. */
  def handler_[W, A](program: Writer[W] ?=> A): A =
    handler(program).result

  /** Handler: run collecting output with a custom combine.
    *
    * Note: A direct implementation would reference `combine` inside the anonymous Writer class, but
    * caps.SharedCapability restricts the self-type to `{cap}` — external references like `combine`
    * are not in the allowed capture set. So we delegate to the list handler and fold afterward.
    */
  def handlerWith[W, A](empty: W)(combine: (W, W) => W)(
      program: Writer[W] ?=> A
  ): (output: W, result: A) =
    val (values, result) = handler(program)
    (output = values.foldLeft(empty)(combine), result = result)
