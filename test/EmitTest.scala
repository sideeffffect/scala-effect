package effect.tests

import effect.effects.Emit

class EmitTest extends munit.FunSuite:

  test("toList collects emitted values"):
    val (values, _) = Emit.toList[Int, Unit]:
      Emit.emit(1)
      Emit.emit(2)
      Emit.emit(3)
    assertEquals(values, List(1, 2, 3))

  test("toList returns result"):
    val (_, v) = Emit.toList[String, Int]:
      Emit.emit("hello")
      42
    assertEquals(v, 42)

  test("fold accumulates values"):
    val (total, _) = Emit.fold[Int, Int, Unit](0)(_ + _):
      Emit.emit(10)
      Emit.emit(20)
      Emit.emit(30)
    assertEquals(total, 60)

  test("fold with string concatenation"):
    val (s, _) = Emit.fold[String, String, Unit]("")(_ + _):
      Emit.emit("a")
      Emit.emit("b")
      Emit.emit("c")
    assertEquals(s, "abc")

  test("mapEmit transforms values"):
    val (values, _) = Emit.toList[Int, Unit]:
      Emit.mapEmit[Int, Unit](_ * 2):
        Emit.emit(1)
        Emit.emit(2)
        Emit.emit(3)
    assertEquals(values, List(2, 4, 6))

  test("mapEmit with string transformation"):
    val (values, _) = Emit.toList[String, Unit]:
      Emit.mapEmit[String, Unit](_.toUpperCase):
        Emit.emit("hello")
        Emit.emit("world")
    assertEquals(values, List("HELLO", "WORLD"))

  test("nested mapEmit composes"):
    val (values, _) = Emit.toList[Int, Unit]:
      Emit.mapEmit[Int, Unit](_ + 100):
        Emit.emit(1)
        Emit.mapEmit[Int, Unit](_ * 10):
          Emit.emit(2)
          Emit.emit(3)
        Emit.emit(4)
    assertEquals(values, List(101, 120, 130, 104))

  test("no emissions produces empty list"):
    val (values, v) = Emit.toList[String, Int](42)
    assertEquals(values, Nil)
    assertEquals(v, 42)

  test("many emissions"):
    val (values, _) = Emit.toList[Int, Unit]:
      for i <- 1 to 100 do Emit.emit(i)
    assertEquals(values, (1 to 100).toList)

  test("emit with complex types"):
    val (values, _) = Emit.toList[Map[String, Int], Unit]:
      Emit.emit(Map("a" -> 1))
      Emit.emit(Map("b" -> 2))
    assertEquals(values, List(Map("a" -> 1), Map("b" -> 2)))
