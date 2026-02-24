package effect.tests

import effect.effects.{Fail, Raise}

class RaiseTest extends munit.FunSuite:

  test("handler returns Right on success"):
    val r = Raise.handler[String, Int](42)
    assertEquals(r, Right(42))

  test("handler returns Left on raise"):
    val r = Raise.handler[String, Int](Raise.raise("boom"))
    assertEquals(r, Left("boom"))

  test("raise short-circuits computation"):
    var reached = false
    val r = Raise.handler[String, Int]:
      Raise.raise("early")
      reached = true
      42
    assertEquals(r, Left("early"))
    assert(!reached)

  test("toOption returns Some on success"):
    val r = Raise.toOption[String, Int](42)
    assertEquals(r, Some(42))

  test("toOption returns None on raise"):
    val r = Raise.toOption[String, Int](Raise.raise("boom"))
    assertEquals(r, None)

  test("catchError recovers from error"):
    val v = Raise.catchError[String, Int](Raise.raise("oops"))(_ => 0)
    assertEquals(v, 0)

  test("catchError passes through success"):
    val v = Raise.catchError[String, Int](42)(_ => 0)
    assertEquals(v, 42)

  test("catchError recovery has access to error"):
    val v = Raise.catchError[String, String](Raise.raise("hello"))(e => s"recovered: $e")
    assertEquals(v, "recovered: hello")

  test("retry re-raises through outer handler"):
    val v = Raise.handler[String, Int]:
      Raise.retry[String, Int] {
        Raise.raise("not yet")
      } { e =>
        Raise.raise(s"retried: $e")
      }
    assertEquals(v, Left("retried: not yet"))

  test("retry passes through success"):
    val v = Raise.handler[String, Int]:
      Raise.retry[String, Int] { 42 } { e => 0 }
    assertEquals(v, Right(42))

  test("nested handlers are independent"):
    val v = Raise.handler[String, Either[Int, Int]]:
      Raise.handler[Int, Int]:
        Raise.raise(42)
    assertEquals(v, Right(Left(42)))

  test("error type can be any type"):
    val r = Raise.handler[List[Int], String](Raise.raise(List(1, 2, 3)))
    assertEquals(r, Left(List(1, 2, 3)))

  test("computation before raise is executed"):
    var counter = 0
    Raise.handler[String, Unit]:
      counter += 1
      counter += 1
      Raise.raise("stop")
      counter += 1
    assertEquals(counter, 2)

  test("handler with complex success value"):
    val r = Raise.handler[String, Map[String, Int]]:
      Map("a" -> 1, "b" -> 2)
    assertEquals(r, Right(Map("a" -> 1, "b" -> 2)))

class FailTest extends munit.FunSuite:

  test("handler returns Some on success"):
    val r = Fail.handler(42)
    assertEquals(r, Some(42))

  test("handler returns None on fail"):
    val r = Fail.handler[Int](Fail.fail())
    assertEquals(r, None)

  test("fail short-circuits"):
    var reached = false
    Fail.handler[Unit]:
      Fail.fail()
      reached = true
    assert(!reached)

  test("nested Fail handlers are independent"):
    val r = Fail.handler[Int]:
      val inner: Option[Int] = Fail.handler[Int](Fail.fail())
      inner.getOrElse(99)
    assertEquals(r, Some(99))
