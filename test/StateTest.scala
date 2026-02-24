package effect.tests

import effect.effects.State

class StateTest extends munit.FunSuite:

  test("get returns initial state"):
    val (s, v) = State.handler(42)(State.get[Int])
    assertEquals(v, 42)
    assertEquals(s, 42)

  test("set changes state"):
    val (s, _) = State.handler(0):
      State.set(99)
    assertEquals(s, 99)

  test("get after set returns new state"):
    val (_, v) = State.handler(0):
      State.set(42)
      State.get[Int]
    assertEquals(v, 42)

  test("modify applies function"):
    val (s, _) = State.handler(10):
      State.modify[Int](_ * 3)
    assertEquals(s, 30)

  test("gets projects from state"):
    val (_, v) = State.handler("hello"):
      State.gets[String, Int](_.length)
    assertEquals(v, 5)

  test("handler_ discards final state"):
    val v = State.handler_(0):
      State.set(42)
      State.get[Int]
    assertEquals(v, 42)

  test("execHandler returns only state"):
    val s = State.execHandler(0):
      State.set(42)
    assertEquals(s, 42)

  test("multiple set/get round-trips"):
    val (s, v) = State.handler(0):
      State.set(1)
      val a = State.get[Int]
      State.set(a + 10)
      val b = State.get[Int]
      State.set(b * 2)
      State.get[Int]
    assertEquals(v, 22)
    assertEquals(s, 22)

  test("increment N times"):
    val (s, _) = State.handler(0):
      for _ <- 1 to 100 do State.modify[Int](_ + 1)
    assertEquals(s, 100)

  test("state with String type"):
    val (s, _) = State.handler(""):
      State.modify[String](_ + "a")
      State.modify[String](_ + "b")
      State.modify[String](_ + "c")
    assertEquals(s, "abc")

  test("state with List accumulation"):
    val (s, _) = State.handler(List.empty[Int]):
      for i <- 1 to 5 do State.modify[List[Int]](i :: _)
    assertEquals(s, List(5, 4, 3, 2, 1))

  test("state with tuple"):
    val (s, _) = State.handler((0, "start")):
      State.modify[(Int, String)]((n, s) => (n + 1, s + "!"))
    assertEquals(s, (1, "start!"))

  test("nested state handlers are independent"):
    val (outer, (inner, _)) = State.handler(0):
      State.handler(100):
        State.modify[Int](_ + 1)
    assertEquals(outer, 0) // outer untouched
    assertEquals(inner, 101) // inner modified
