package effect.tests

import effect.effects.Amb

class AmbTest extends munit.FunSuite:

  test("choose from alternatives"):
    val r = Amb.handler(Amb.choose(List(1, 2, 3)))
    assertEquals(r.toSet, Set(1, 2, 3))

  test("empty produces no results"):
    val r = Amb.handler[Int](Amb.empty)
    assertEquals(r, Nil)

  test("guard filters"):
    def program(using Amb): Int =
      val x = Amb.choose(List(1, 2, 3, 4, 5))
      Amb.guard(x % 2 == 0)
      x

    val r = Amb.handler(program)
    assertEquals(r.toSet, Set(2, 4))

  test("cut prunes remaining siblings"):
    def program(using Amb): Int =
      val x = Amb.choose(List(1, 2, 3, 4, 5))
      if x >= 3 then Amb.cut()
      x

    val r = Amb.handler(program)
    assert(r.contains(1))
    assert(r.contains(2))
    assert(r.contains(3))
    assert(!r.contains(4))
    assert(!r.contains(5))

  test("chooseAndCut takes first match"):
    val r = Amb.handler:
      val x = Amb.chooseAndCut(List("a", "b", "c"))
      s"got: $x"
    assertEquals(r, List("got: a"))

  test("handlerFirst returns only first result"):
    val r = Amb.handlerFirst(Amb.choose(List(10, 20, 30)))
    assertEquals(r, Some(10))

  test("handlerFirst returns None on empty"):
    val r = Amb.handlerFirst[Int](Amb.empty)
    assertEquals(r, None)

  test("two choices produce combinations"):
    def program(using Amb): (Int, String) =
      val x = Amb.choose(List(1, 2))
      val y = Amb.choose(List("a", "b"))
      (x, y)

    val r = Amb.handler(program)
    assertEquals(r.toSet, Set((1, "a"), (1, "b"), (2, "a"), (2, "b")))

  test("guard with multiple choices"):
    def program(using Amb): (Int, Int) =
      val x = Amb.choose((1 to 5).toList)
      val y = Amb.choose((1 to 5).toList)
      Amb.guard(x + y == 5)
      (x, y)

    val r = Amb.handler(program)
    assertEquals(r.toSet, Set((1, 4), (2, 3), (3, 2), (4, 1)))
