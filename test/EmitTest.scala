package effect.tests

import effect.effects.Emit

class EmitTest extends munit.FunSuite:

  test("toList collects emitted values"):
    def program(using Emit[Int]): Unit =
      Emit.emit(1)
      Emit.emit(2)
      Emit.emit(3)

    val (values, _) = Emit.toList(program)
    assertEquals(values, List(1, 2, 3))

  test("toList returns result"):
    val (_, v) = Emit.toList[String, Int]:
      Emit.emit("hello")
      42
    assertEquals(v, 42)

  test("fold accumulates values"):
    def program(using Emit[Int]): Unit =
      Emit.emit(10)
      Emit.emit(20)
      Emit.emit(30)

    val (total, _) = Emit.fold[Int, Int, Unit](0)(_ + _)(program)
    assertEquals(total, 60)

  test("fold with string concatenation"):
    def program(using Emit[String]): Unit =
      Emit.emit("a")
      Emit.emit("b")
      Emit.emit("c")

    val (s, _) = Emit.fold[String, String, Unit]("")(_ + _)(program)
    assertEquals(s, "abc")

  test("mapEmit transforms values"):
    def program(using Emit[Int]): Unit =
      Emit.mapEmit[Int, Unit](_ * 2):
        Emit.emit(1)
        Emit.emit(2)
        Emit.emit(3)

    val (values, _) = Emit.toList(program)
    assertEquals(values, List(2, 4, 6))

  test("mapEmit with string transformation"):
    def program(using Emit[String]): Unit =
      Emit.mapEmit[String, Unit](_.toUpperCase):
        Emit.emit("hello")
        Emit.emit("world")

    val (values, _) = Emit.toList(program)
    assertEquals(values, List("HELLO", "WORLD"))

  test("nested mapEmit composes"):
    def program(using Emit[Int]): Unit =
      Emit.mapEmit[Int, Unit](_ + 100):
        Emit.emit(1)
        Emit.mapEmit[Int, Unit](_ * 10):
          Emit.emit(2)
          Emit.emit(3)
        Emit.emit(4)

    val (values, _) = Emit.toList(program)
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
    def program(using Emit[Map[String, Int]]): Unit =
      Emit.emit(Map("a" -> 1))
      Emit.emit(Map("b" -> 2))

    val (values, _) = Emit.toList(program)
    assertEquals(values, List(Map("a" -> 1), Map("b" -> 2)))
