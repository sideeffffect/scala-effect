package effect.tests

import effect.effects.Console

class ConsoleTest extends munit.FunSuite:

  test("pureHandler captures output"):
    val (_, output, _) = Console.pureHandler(Nil):
      Console.printLine("hello")
      Console.printLine("world")
    assertEquals(output, List("hello", "world"))

  test("pureHandler provides input"):
    val (remaining, _, v) = Console.pureHandler(List("alice")):
      Console.readLine()
    assertEquals(v, "alice")
    assertEquals(remaining, Nil)

  test("pureHandler returns remaining input"):
    val (remaining, _, _) = Console.pureHandler(List("a", "b", "c")):
      Console.readLine()
    assertEquals(remaining, List("b", "c"))

  test("readLine from empty input returns empty string"):
    val (_, _, v) = Console.pureHandler(Nil):
      Console.readLine()
    assertEquals(v, "")

  test("testHandler returns output and result"):
    val (output, v) = Console.testHandler(List("42")):
      val input = Console.readLine()
      Console.printLine(s"got: $input")
      input.toInt
    assertEquals(output, List("got: 42"))
    assertEquals(v, 42)

  test("echo program"):
    def echo(using Console): Unit =
      val s = Console.readLine()
      if s.nonEmpty then
        Console.printLine(s)
        echo

    val (output, _) = Console.testHandler(List("hello", "world", "")):
      echo
    assertEquals(output, List("hello", "world"))

  test("interactive round-trip"):
    val (output, v) = Console.testHandler(List("yes", "42")):
      Console.printLine("Continue? (yes/no)")
      val answer = Console.readLine()
      if answer == "yes" then
        Console.printLine("Enter a number:")
        Console.readLine().toInt
      else 0
    assertEquals(output, List("Continue? (yes/no)", "Enter a number:"))
    assertEquals(v, 42)

  test("no output produces empty list"):
    val (output, v) = Console.testHandler(Nil)(42)
    assertEquals(output, Nil)
    assertEquals(v, 42)

  test("multiple reads consume input in order"):
    val (_, _, v) = Console.pureHandler(List("a", "b", "c")):
      val x = Console.readLine()
      val y = Console.readLine()
      val z = Console.readLine()
      (x, y, z)
    assertEquals(v, ("a", "b", "c"))
