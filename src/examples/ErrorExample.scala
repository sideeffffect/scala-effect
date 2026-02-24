package effect.examples

import effect.effects.{Fail, Raise, State}

/** Error/exception examples — port of Effective's except examples.
  *
  * Effective version: monus :: Int -> Int -> Int ! '[Throw] monus x y = if x < y then throw else
  * return (x - y)
  *
  * safeMonus :: Int -> Int -> Prog '[Throw, Catch] Int safeMonus x y = catch (monus x y) (return 0)
  *
  * ghci> handle except (monus 1 5) -- Nothing ghci> handle except (safeMonus 1 5) -- Just 0
  */

/** Natural number subtraction: fails if result would be negative. */
def monus(x: Int, y: Int)(using Raise[String]): Int =
  if x < y then Raise.raise(s"$x < $y: cannot subtract")
  else x - y

/** Safe subtraction: catches errors and returns 0. */
def safeMonus(x: Int, y: Int): Int =
  Raise.catchError(monus(x, y))(_ => 0)

/** Division with error handling. */
def safeDiv(x: Int, y: Int)(using Raise[String]): Int =
  if y == 0 then Raise.raise("division by zero")
  else x / y

/** Composing error with state — demonstrates handler ordering.
  *
  * In Effective, handler ordering matters: state s `fuse` except -- "global state": state survives
  * errors except `fuse` state s -- "local state": state rolls back on error
  *
  * In Scala, this is just nesting order: State.handler { Raise.handler { ... } } -- state outer =
  * global Raise.handler { State.handler { ... } } -- error outer = local
  */
def tickAndFail(using State[Int], Raise[String]): Int =
  State.modify[Int](_ + 1) // tick: 0 -> 1
  State.modify[Int](_ + 1) // tick: 1 -> 2
  Raise.raise("boom") // fail

@main def errorExamples(): Unit =
  // Basic error handling
  println(s"monus(5, 3) = ${Raise.handler(monus(5, 3))}")
  // Expected: Right(2)

  println(s"monus(1, 5) = ${Raise.handler(monus(1, 5))}")
  // Expected: Left("1 < 5: cannot subtract")

  println(s"safeMonus(1, 5) = ${safeMonus(1, 5)}")
  // Expected: 0

  // Catch and retry
  val recovered = Raise.catchError(safeDiv(10, 0))(_ => -1)
  println(s"safeDiv(10, 0) recovered: $recovered")
  // Expected: -1

  // Global state: State handler outside Error handler
  // State persists even when error occurs
  val (globalState, errorResult) =
    State.handler(0):
      Raise.handler[String, Int]:
        tickAndFail
  println(s"global state: state=$globalState, result=$errorResult")
  // Expected: state=2, result=Left("boom")

  // Local state: Error handler outside State handler
  // State rolls back when error occurs
  val localResult: Either[String, (Int, Int)] =
    Raise.handler[String, (Int, Int)]:
      State.handler(0):
        tickAndFail
  println(s"local state: $localResult")
  // Expected: Left("boom") — state is lost
