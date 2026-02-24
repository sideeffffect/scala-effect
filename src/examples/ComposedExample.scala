package effect.examples

import effect.effects.*

/** Composed effects example — demonstrates handler composition.
  *
  * In Effective, handler composition uses explicit combinators: teletype str = getLinePure_ str |>
  * putStrLnPure globalState s = except `fuse` state s
  *
  * In Scala, composition is simply nested handler application. The nesting order determines
  * semantics, just like in Effective.
  */

/** A guessing game using Reader (secret), State (attempts), Console, and Raise. */
def guessingGame(using Reader[Int], State[Int], Console, Raise[String]): Unit =
  val secret = Reader.ask[Int]
  Console.printLine("Guess the number (1-100):")

  val input = Console.readLine()
  val guess = input.toIntOption.getOrElse:
    Raise.raise(s"Invalid input: $input")

  State.modify[Int](_ + 1)
  val attempts = State.get[Int]

  if guess == secret then Console.printLine(s"Correct! You got it in $attempts attempts.")
  else if guess < secret then
    Console.printLine("Too low!")
    guessingGame
  else
    Console.printLine("Too high!")
    guessingGame

/** A computation combining Writer and State — counting and logging.
  *
  * Demonstrates the Effective pattern of composing independent handlers: ticker = tickState ||>
  * state (0 :: Int)
  */
def tickingLog(items: List[String])(using Writer[String], State[Int]): List[String] =
  items.map: item =>
    State.modify[Int](_ + 1)
    val count = State.get[Int]
    val processed = s"[$count] ${item.toUpperCase}"
    Writer.tell(s"Processed: $processed")
    processed

/** Emit + State: generator with counter. */
def numberedEmit(items: List[String])(using Emit[String], State[Int]): Unit =
  items.foreach: item =>
    State.modify[Int](_ + 1)
    val n = State.get[Int]
    Emit.emit(s"$n. $item")

@main def composedExamples(): Unit =
  // Guessing game with pure handlers (for testing)
  val result = Reader.handler(42):
    State.handler(0):
      Console.testHandler(List("50", "25", "42")):
        Raise.handler[String, Unit]:
          guessingGame
  println("Guessing game result:")
  val (_, (output, errorOrUnit)) = result
  println(s"  Console output: $output")
  println(s"  Result: $errorOrUnit")

  println()

  // Ticking log
  val (log, (count, processed)) =
    Writer.handler:
      State.handler(0):
        tickingLog(List("hello", "world", "scala"))
  println("Ticking log:")
  println(s"  Log: $log")
  println(s"  Count: $count")
  println(s"  Processed: $processed")

  println()

  // Numbered emit
  val (emitted, (count2, _)) =
    Emit.toList:
      State.handler(0):
        numberedEmit(List("alpha", "beta", "gamma"))
  println("Numbered emit:")
  emitted.foreach(s => println(s"  $s"))
  println(s"  Final count: $count2")
