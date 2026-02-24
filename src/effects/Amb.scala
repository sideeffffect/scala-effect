package effect.effects

import caps.SharedCapability

/** Amb effect — ambiguity/nondeterministic choice with cut support.
  *
  * Corresponds to Effective's Alternative + Cut effects:
  *   - Empty + Choose (Alternative)
  *   - CutFail + CutCall (Cut pruning)
  *
  * Amb extends Nondet with search control: `cut` prunes all remaining siblings at the current
  * choice point, implementing Prolog-style cut.
  *
  * In Effective: cut :: Members [Empty, Choose, CutFail] sig => Prog sig () cutCall :: Member
  * CutCall sig => Prog sig a -> Prog sig a
  */
trait Amb extends SharedCapability:
  def empty[A](): A
  def choose[A](alternatives: List[A]): A
  def cut(): Unit

object Amb:

  def empty[A](using a: Amb): A = a.empty()
  def choose[A](alternatives: List[A])(using a: Amb): A = a.choose(alternatives)
  def cut()(using a: Amb): Unit = a.cut()

  /** Guard: fail the current branch if condition is false. */
  def guard(condition: Boolean)(using a: Amb): Unit =
    if !condition then a.empty()

  /** Choose and cut: pick the first alternative that succeeds, then cut. */
  def chooseAndCut[A](alternatives: List[A])(using a: Amb): A =
    val result = a.choose(alternatives)
    a.cut()
    result

  private case object EmptySignal extends Exception(null, null, true, false)
  private case object CutSignal extends Exception(null, null, true, false)

  /** Handler: collect all results with cut support.
    *
    * Corresponds to Effective's: cutList :: Prog '[Empty, Choose, Once] a -> [a] backtrack ::
    * Handler '[Empty, Choose, Once] '[] '[CutListT] a [a]
    *
    * Cut prunes remaining siblings at the current choice point. Uses the same re-execution strategy
    * as Nondet.handler, extended with a "cut" flag that prunes the worklist when triggered.
    */
  def handler[A](program: Amb ?=> A): List[A] =
    val results = collection.mutable.ListBuffer.empty[A]
    val worklist = collection.mutable.Queue.empty[Vector[Int]]
    worklist.enqueue(Vector.empty)

    // Track which choice indices have been cut at each depth
    val cutAtDepth = collection.mutable.Set.empty[Int]

    while worklist.nonEmpty do
      val path = worklist.dequeue()
      var choiceIndex = 0
      val actualPath = collection.mutable.ArrayBuffer.empty[Int]
      var cutTriggered = false
      var cutDepth = -1

      val cap = new Amb:
        def empty[B](): B = throw EmptySignal
        def choose[B](alternatives: List[B]): B =
          val myIndex = choiceIndex
          choiceIndex += 1
          alternatives match
            case Nil      => throw EmptySignal
            case h :: Nil =>
              actualPath += 0
              h
            case _ =>
              if cutAtDepth.contains(myIndex) then
                // This choice point was cut: only take the first alternative
                actualPath += 0
                alternatives.head
              else
                val idx = if myIndex < path.length then path(myIndex) else 0
                if idx >= alternatives.length then throw EmptySignal
                actualPath += idx
                if myIndex >= path.length then
                  val prefix = actualPath.toVector.init
                  for i <- 1 until alternatives.length do worklist.enqueue(prefix :+ i)
                alternatives(idx)

        def cut(): Unit =
          // Mark the most recent choice point as cut
          if actualPath.nonEmpty then
            cutTriggered = true
            cutDepth = choiceIndex - 1
            cutAtDepth += cutDepth
            // Remove worklist entries that would explore siblings at this depth
            val currentPrefix = actualPath.toVector.init
            worklist.filterInPlace: path =>
              !(path.length > cutDepth &&
                path.take(cutDepth) == currentPrefix.take(cutDepth))

      try
        val result = program(using cap)
        results += result
      catch case _: EmptySignal.type => ()

    results.toList

  /** Handler that returns only the first result (like Prolog's once/1). */
  def handlerFirst[A](program: Amb ?=> A): Option[A] =
    handler(program).headOption
