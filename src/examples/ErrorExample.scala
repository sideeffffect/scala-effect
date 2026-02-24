package effect.examples

import effect.effects.{ArithmeticError, EffectError, Fail, Raise, State}

/** Error/exception examples — port of Effective's except examples. */

/** Natural number subtraction: fails if result would be negative. */
def monus(x: Int, y: Int)(using Raise[ArithmeticError]): Int =
  if x < y then Raise.raise(ArithmeticError(s"$x < $y: cannot subtract"))
  else x - y

/** Safe subtraction: catches errors and returns 0. */
def safeMonus(x: Int, y: Int): Int =
  Raise.catchError(monus(x, y))(_ => 0)

/** Division with error handling. */
def safeDiv(x: Int, y: Int)(using Raise[ArithmeticError]): Int =
  if y == 0 then Raise.raise(ArithmeticError("division by zero"))
  else x / y

/** Composing error with state — demonstrates handler ordering. */
def tickAndFail(using State[Int], Raise[EffectError]): Int =
  State.modify[Int](_ + 1)
  State.modify[Int](_ + 1)
  Raise.raise(EffectError("boom"))

@main def errorExamples(): Unit =
  println(s"monus(5, 3) = ${Raise.handler(monus(5, 3))}")
  // Expected: Right(2)

  println(s"monus(1, 5) = ${Raise.handler(monus(1, 5))}")
  // Expected: Left(ArithmeticError(1 < 5: cannot subtract))

  println(s"safeMonus(1, 5) = ${safeMonus(1, 5)}")
  // Expected: 0

  val recovered = Raise.catchError(safeDiv(10, 0))(_ => -1)
  println(s"safeDiv(10, 0) recovered: $recovered")
  // Expected: -1

  // Global state: State handler outside Error handler
  val (globalState, errorResult) =
    State.handler(0):
      Raise.handler[EffectError, Int]:
        tickAndFail
  println(s"global state: state=$globalState, result=$errorResult")
  // Expected: state=2, result=Left(EffectError(boom))

  // Local state: Error handler outside State handler
  val localResult: Either[EffectError, (Int, Int)] =
    Raise.handler[EffectError, (Int, Int)]:
      State.handler(0):
        tickAndFail
  println(s"local state: $localResult")
  // Expected: Left(EffectError(boom))
