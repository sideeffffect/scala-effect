package effect.tests

import effect.effects.Nondet

class NondetTest extends munit.FunSuite:

  test("choose from single element"):
    val r = Nondet.handler(Nondet.choose(List(42)))
    assertEquals(r, List(42))

  test("choose from multiple elements"):
    val r = Nondet.handler(Nondet.choose(List(1, 2, 3)))
    assertEquals(r, List(1, 2, 3))

  test("empty produces no results"):
    val r = Nondet.handler[Int](Nondet.empty)
    assertEquals(r, List.empty[Int])

  test("choose then filter"):
    val r = Nondet.handler[Int]:
      val x = Nondet.choose(List(1, 2, 3, 4, 5))
      if x % 2 == 0 then x else Nondet.empty
    assertEquals(r, List(2, 4))

  test("two choices produce cartesian product"):
    val r = Nondet.handler[(Int, String)]:
      val x = Nondet.choose(List(1, 2))
      val y = Nondet.choose(List("a", "b"))
      (x, y)
    assertEquals(r.toSet, Set((1, "a"), (1, "b"), (2, "a"), (2, "b")))
    assertEquals(r.length, 4)

  test("three choices produce full combinations"):
    val r = Nondet.handler[(Int, Int, Int)]:
      val a = Nondet.choose(List(0, 1))
      val b = Nondet.choose(List(0, 1))
      val c = Nondet.choose(List(0, 1))
      (a, b, c)
    assertEquals(r.length, 8) // 2^3
    assert(r.contains((0, 0, 0)))
    assert(r.contains((1, 1, 1)))

  test("alt explores both branches"):
    val r = Nondet.handler[String]:
      Nondet.alt("left", "right")
    assertEquals(r, List("left", "right"))

  test("knapsack example from Effective"):
    def knapsack(w: Int, vs: List[Int])(using Nondet): List[Int] =
      if w < 0 then Nondet.empty
      else if w == 0 then Nil
      else
        val v = Nondet.choose(vs)
        v :: knapsack(w - v, vs)

    val r = Nondet.handler(knapsack(3, List(3, 2, 1)))
    assertEquals(r, List(List(3), List(2, 1), List(1, 2), List(1, 1, 1)))

  test("choose from empty list produces no results"):
    val r = Nondet.handler[Int](Nondet.choose(Nil))
    assertEquals(r, Nil)

  test("early empty prunes branch"):
    var counter = 0
    val r = Nondet.handler[Int]:
      Nondet.empty[Unit]
      counter += 1 // should never run
      42
    assertEquals(r, Nil)
    assertEquals(counter, 0)

  test("oneOf selects from range"):
    val r = Nondet.handler(Nondet.oneOf(1 to 3))
    assertEquals(r, List(1, 2, 3))

  test("choice-dependent branches"):
    val r = Nondet.handler[Int]:
      val x = Nondet.choose(List(1, 2, 3))
      val y = Nondet.choose((1 to x).toList)
      x * 10 + y
    assertEquals(r.toSet, Set(11, 21, 22, 31, 32, 33))
    assertEquals(r.length, 6)

  test("pythagorean triples"):
    val r = Nondet.handler[(Int, Int, Int)]:
      val a = Nondet.choose((1 to 10).toList)
      val b = Nondet.choose((a to 10).toList)
      val c = Nondet.choose((b to 10).toList)
      if a * a + b * b == c * c then (a, b, c)
      else Nondet.empty
    assertEquals(r, List((3, 4, 5), (6, 8, 10)))

  test("handler with pure computation (no choices)"):
    val r = Nondet.handler(42)
    assertEquals(r, List(42))

  test("sequential choices with filtering"):
    val r = Nondet.handler[Int]:
      val x = Nondet.choose(List(1, 2, 3))
      if x == 2 then Nondet.empty
      x * 10
    assertEquals(r, List(10, 30))
