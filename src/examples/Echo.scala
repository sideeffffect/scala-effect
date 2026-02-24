package effect.examples

import effect.effects.Console

/** Echo program — direct port of Effective's echo example.
  *
  * Effective version: echo :: Members '[GetLine, PutStrLn] sig => Prog sig () echo = do str <-
  * getLine case str of [] -> return () _ -> do putStrLn str; echo
  *
  * Scala version uses Console capability with context function.
  */
def echo(using Console): Unit =
  val str = Console.readLine()
  if str.nonEmpty then
    Console.printLine(str)
    echo

/** Run echo with live IO. */
@main def echoLive(): Unit =
  Console.liveHandler(echo)

/** Run echo with pure/test handler. */
@main def echoTest(): Unit =
  val (output, _) = Console.testHandler(List("Hello", "World", "")):
    echo
  println(s"Echo output: $output")
  // Expected: List("Hello", "World")
