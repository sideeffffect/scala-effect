package effect.effects

import caps.SharedCapability

/** Amb effect — ambiguity/nondeterministic choice with cut support.
  *
  * Amb extends Nondet with search control: `cut` prunes all remaining siblings at the current
  * choice point, implementing Prolog-style cut.
  */
trait Amb extends SharedCapability:
  def empty[A](): A
  def choose[A](alternatives: List[A]): A
  def cut(): Unit

object Amb:

  inline def empty[A](using Amb): A = summon[Amb].empty()
  inline def choose[A](alternatives: List[A])(using Amb): A = summon[Amb].choose(alternatives)
  inline def cut()(using Amb): Unit = summon[Amb].cut()

  inline def guard(condition: Boolean)(using Amb): Unit =
    if !condition then summon[Amb].empty()

  inline def chooseAndCut[A](alternatives: List[A])(using Amb): A =
    val a = summon[Amb]
    val result = a.choose(alternatives)
    a.cut()
    result

  private case object EmptySignal extends Exception(null, null, true, false)

  /** Handler: collect all results with cut support.
    *
    * Cut prunes remaining siblings at the current choice point. Uses the same re-execution strategy
    * as Nondet.handler, extended with a "cut" flag that prunes the worklist when triggered.
    */
  def handler[A](program: Amb ?=> A): List[A] =
    val results = collection.mutable.ListBuffer.empty[A]
    val worklist = collection.mutable.Queue.empty[Vector[Int]]
    worklist.enqueue(Vector.empty)

    val cutAtDepth = collection.mutable.Set.empty[Int]

    while worklist.nonEmpty do
      val path = worklist.dequeue()
      var choiceIndex = 0
      val actualPath = collection.mutable.ArrayBuffer.empty[Int]

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
          if actualPath.nonEmpty then
            val depth = choiceIndex - 1
            cutAtDepth += depth
            val currentPrefix = actualPath.toVector.init
            worklist.filterInPlace: path =>
              !(path.length > depth &&
                path.take(depth) == currentPrefix.take(depth))

      try
        val result = program(using cap)
        results += result
      catch case _: EmptySignal.type => ()

    results.toList

  /** Handler that returns only the first result (like Prolog's once/1). */
  def handlerFirst[A](program: Amb ?=> A): Option[A] =
    handler(program).headOption
