package effect.tests

import effect.effects.Timeout
import java.time.Duration

class TimeoutTest extends munit.FunSuite:

  test("handler returns Some when within deadline"):
    val r = Timeout.handler(Duration.ofSeconds(1))(42)
    assertEquals(r, Some(42))

  test("handler returns None when checkTimeout after expiry"):
    def program(using Timeout): Int =
      Thread.sleep(50)
      Timeout.checkTimeout()
      42

    val r = Timeout.handler(Duration.ofMillis(10))(program)
    assertEquals(r, None)

  test("remaining decreases over time"):
    def program(using Timeout): Boolean =
      val r1 = Timeout.remaining
      Thread.sleep(50)
      val r2 = Timeout.remaining
      r1.compareTo(r2) > 0

    val r = Timeout.handler(Duration.ofSeconds(10))(program)
    assertEquals(r, Some(true))

  test("isExpired is false before deadline"):
    val r = Timeout.handler(Duration.ofSeconds(10))(Timeout.isExpired)
    assertEquals(r, Some(false))

  test("isExpired is true after deadline"):
    def program(using Timeout): Boolean =
      Thread.sleep(20)
      Timeout.isExpired

    val r = Timeout.handler(Duration.ofMillis(1))(program)
    assertEquals(r, Some(true))

  test("handlerEither returns Right on success"):
    val r = Timeout.handlerEither(Duration.ofSeconds(1))("ok")
    assert(r.isRight)
    assertEquals(r.toOption, Some("ok"))

  test("handlerEither returns Left on timeout"):
    def program(using Timeout): String =
      Thread.sleep(50)
      Timeout.checkTimeout()
      "never"

    val r = Timeout.handlerEither(Duration.ofMillis(10))(program)
    assert(r.isLeft)

  test("multiple checkTimeout calls, fail on second"):
    var checks = 0
    def program(using Timeout): Int =
      checks += 1
      Timeout.checkTimeout()
      Thread.sleep(60)
      checks += 1
      Timeout.checkTimeout()
      checks += 1
      42

    val r = Timeout.handler(Duration.ofMillis(30))(program)
    assertEquals(r, None)
    assertEquals(checks, 2)
