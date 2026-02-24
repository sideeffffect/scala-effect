package effect.tests

import effect.effects.Reader

class ReaderTest extends munit.FunSuite:

  test("ask returns environment"):
    val v = Reader.handler(42)(Reader.ask[Int])
    assertEquals(v, 42)

  test("asks projects from environment"):
    val v = Reader.handler("hello"):
      Reader.asks[String, Int](_.length)
    assertEquals(v, 5)

  test("local modifies environment for block"):
    val v = Reader.handler(10):
      val before = Reader.ask[Int]
      val inside = Reader.local[Int, Int](_ * 2)(Reader.ask[Int])
      val after = Reader.ask[Int]
      (before, inside, after)
    assertEquals(v, (10, 20, 10))

  test("nested local composes"):
    val v = Reader.handler(1):
      Reader.local[Int, Int](_ + 10):
        Reader.local[Int, Int](_ * 3):
          Reader.ask[Int]
    assertEquals(v, 33) // (1 + 10) * 3

  test("local with different return type"):
    val v = Reader.handler("world"):
      Reader.local[String, String]("hello " + _):
        Reader.ask[String]
    assertEquals(v, "hello world")

  test("ask used multiple times returns same value"):
    val v = Reader.handler(42):
      val a = Reader.ask[Int]
      val b = Reader.ask[Int]
      val c = Reader.ask[Int]
      (a, b, c)
    assertEquals(v, (42, 42, 42))

  test("nested handlers with different types"):
    val v = Reader.handler(42):
      Reader.handler("hello"):
        val s = Reader.ask[String]
        s
    assertEquals(v, "hello")

  test("local restores environment after error-like situation"):
    val v = Reader.handler(1):
      val a = Reader.local[Int, Int](_ + 100):
        Reader.ask[Int]
      val b = Reader.ask[Int]
      (a, b)
    assertEquals(v, (101, 1))

  test("deeply nested locals"):
    val v = Reader.handler(0):
      Reader.local[Int, Int](_ + 1):
        Reader.local[Int, Int](_ + 1):
          Reader.local[Int, Int](_ + 1):
            Reader.local[Int, Int](_ + 1):
              Reader.local[Int, Int](_ + 1):
                Reader.ask[Int]
    assertEquals(v, 5)

  test("local with identity preserves environment"):
    val v = Reader.handler(42):
      Reader.local[Int, Int](identity):
        Reader.ask[Int]
    assertEquals(v, 42)
