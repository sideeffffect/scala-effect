package effect.tests

import effect.effects.{EffectError, Fail, Raise, ValidationError}

class RaiseTest extends munit.FunSuite:

  test("handler returns Right on success"):
    val r = Raise.handler[EffectError, Int](42)
    assertEquals(r, Right(42))

  test("handler returns Left on raise"):
    val r = Raise.handler[EffectError, Int](Raise.raise(EffectError("boom")))
    assertEquals(r, Left(EffectError("boom")))

  test("raise short-circuits computation"):
    var reached = false
    val r = Raise.handler[EffectError, Int]:
      Raise.raise(EffectError("early"))
      reached = true
      42
    assertEquals(r, Left(EffectError("early")))
    assert(!reached)

  test("toOption returns Some on success"):
    val r = Raise.toOption[EffectError, Int](42)
    assertEquals(r, Some(42))

  test("toOption returns None on raise"):
    val r = Raise.toOption[EffectError, Int](Raise.raise(EffectError("boom")))
    assertEquals(r, None)

  test("catchError recovers from error"):
    val v = Raise.catchError[EffectError, Int](Raise.raise(EffectError("oops")))(_ => 0)
    assertEquals(v, 0)

  test("catchError passes through success"):
    val v = Raise.catchError[EffectError, Int](42)(_ => 0)
    assertEquals(v, 42)

  test("catchError recovery has access to error"):
    val v = Raise.catchError[EffectError, String](Raise.raise(EffectError("hello")))(e =>
      s"recovered: ${e.msg}"
    )
    assertEquals(v, "recovered: hello")

  test("retry re-raises through outer handler"):
    val v = Raise.handler[EffectError, Int]:
      Raise.retry[EffectError, Int] {
        Raise.raise(EffectError("not yet"))
      } { e =>
        Raise.raise(EffectError(s"retried: ${e.msg}"))
      }
    assertEquals(v, Left(EffectError("retried: not yet")))

  test("retry passes through success"):
    val v = Raise.handler[EffectError, Int]:
      Raise.retry[EffectError, Int] { 42 } { _ => 0 }
    assertEquals(v, Right(42))

  test("nested handlers are independent"):
    val v = Raise.handler[EffectError, Either[ValidationError, Int]]:
      Raise.handler[ValidationError, Int]:
        Raise.raise(ValidationError("inner"))
    assertEquals(v, Right(Left(ValidationError("inner"))))

  test("error type can be any exception subtype"):
    val r = Raise.handler[ValidationError, String](Raise.raise(ValidationError("bad")))
    assertEquals(r, Left(ValidationError("bad")))

  test("computation before raise is executed"):
    var counter = 0
    Raise.handler[EffectError, Unit]:
      counter += 1
      counter += 1
      Raise.raise(EffectError("stop"))
      counter += 1
    assertEquals(counter, 2)

  test("handler with complex success value"):
    val r = Raise.handler[EffectError, Map[String, Int]]:
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
