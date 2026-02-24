package effect.examples

import effect.effects.State

/** State example — direct port of Effective's state examples.
  *
  * Effective version: incr :: () ! '[Put Int, Get Int] incr = do { x <- get; put @Int (x + 1) }
  *
  * ghci> handle (state 41) incr (42, ())
  */
def incr(using State[Int]): Unit =
  val x = State.get
  State.set(x + 1)

/** Counter: increment N times. */
def count(n: Int)(using State[Int]): Unit =
  for _ <- 0 until n do incr

/** Fibonacci using state — demonstrates using State with a tuple. */
def fib(n: Int)(using State[(Int, Int)]): Int =
  for _ <- 0 until n do
    val (a, b) = State.get
    State.set((b, a + b))
  State.get._1

@main def stateExamples(): Unit =
  // Basic increment
  val (finalState, _) = State.handler(41)(incr)
  println(s"incr from 41: $finalState")
  // Expected: 42

  // Count 10 times from 0
  val (count10, _) = State.handler(0)(count(10))
  println(s"count(10) from 0: $count10")
  // Expected: 10

  // Fibonacci
  val (_, fib10) = State.handler((0, 1))(fib(10))
  println(s"fib(10): $fib10")
  // Expected: 55

  // Composing state with other state (nested handlers)
  val (outerState, (innerState, _)) =
    State.handler(0):
      State.handler(100):
        // Inner state starts at 100, outer at 0
        val inner: Int = State.get[Int] // gets inner (100)
        // We can't easily get outer here without differentiated types
        State.set(inner + 1)
  println(s"nested state: outer=$outerState, inner=$innerState")
  // Expected: outer=0, inner=101
