package effect.effects

import caps.SharedCapability

/** Nondeterminism effect — corresponds to Effective's Empty/Choose effects.
  *
  * Haskell's Effective uses multi-shot continuations (via the Prog monad) to explore all branches.
  * Scala doesn't have multi-shot continuations, so we use a **re-execution** strategy: the program
  * is run multiple times, each time with a different selection at each choice point.
  */
trait Nondet extends SharedCapability:
  def empty[A](): A
  def choose[A](alternatives: List[A]): A

object Nondet:

  inline def empty[A](using Nondet): A = summon[Nondet].empty()

  inline def choose[A](alternatives: List[A])(using Nondet): A =
    summon[Nondet].choose(alternatives)

  inline def alt[A](lhs: => A, rhs: => A)(using Nondet): A =
    if summon[Nondet].choose(List(true, false)) then lhs else rhs

  inline def oneOf(range: Range)(using Nondet): Int =
    summon[Nondet].choose(range.toList)

  private case object EmptySignal extends Exception(null, null, true, false)

  /** Handler: collect all results from a nondeterministic computation.
    *
    * Strategy: worklist-based exploration. Each worklist entry is a "path" of choice indices. We
    * track the path built during execution and schedule sibling alternatives for new choice points.
    */
  def handler[A](program: Nondet ?=> A): List[A] =
    val results = collection.mutable.ListBuffer.empty[A]
    val worklist = collection.mutable.Queue.empty[Vector[Int]]
    worklist.enqueue(Vector.empty)

    while worklist.nonEmpty do
      val path = worklist.dequeue()
      var choiceIndex = 0
      val actualPath = collection.mutable.ArrayBuffer.empty[Int]

      val cap = new Nondet:
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
              val idx =
                if myIndex < path.length then path(myIndex) else 0
              if idx >= alternatives.length then throw EmptySignal
              actualPath += idx
              if myIndex >= path.length then
                val prefix = actualPath.toVector.init
                for i <- 1 until alternatives.length do worklist.enqueue(prefix :+ i)
              alternatives(idx)

      try
        val result = program(using cap)
        results += result
      catch case _: EmptySignal.type => ()

    results.toList
