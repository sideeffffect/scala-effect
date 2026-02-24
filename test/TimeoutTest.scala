package effect.tests

import effect.effects.Timeout
import java.time.Duration

class TimeoutTest extends munit.FunSuite:

  test("handler returns Some when within deadline"):
    val r = Timeout.handler(Duration.ofSeconds(1)):
      42
    assertEquals(r, Some(42))

  test("handler returns None when checkTimeout after expiry"):
    val r = Timeout.handler(Duration.ofMillis(10)):
      Thread.sleep(50)
      Timeout.checkTimeout()
      42
    assertEquals(r, None)

  test("remaining decreases over time"):
    val r = Timeout.handler(Duration.ofSeconds(10)):
      val r1 = Timeout.remaining
      Thread.sleep(50)
      val r2 = Timeout.remaining
      r1.compareTo(r2) > 0 // r1 > r2 (more time remaining earlier)
    assertEquals(r, Some(true))

  test("isExpired is false before deadline"):
    val r = Timeout.handler(Duration.ofSeconds(10)):
      Timeout.isExpired
    assertEquals(r, Some(false))

  test("isExpired is true after deadline"):
    val r = Timeout.handler(Duration.ofMillis(1)):
      Thread.sleep(20)
      Timeout.isExpired
    assertEquals(r, Some(true))

  test("handlerEither returns Right on success"):
    val r = Timeout.handlerEither(Duration.ofSeconds(1)):
      "ok"
    assert(r.isRight)
    assertEquals(r.toOption, Some("ok"))

  test("handlerEither returns Left on timeout"):
    val r = Timeout.handlerEither(Duration.ofMillis(10)):
      Thread.sleep(50)
      Timeout.checkTimeout()
      "never"
    assert(r.isLeft)

  test("multiple checkTimeout calls, fail on second"):
    var checks = 0
    val r = Timeout.handler(Duration.ofMillis(30)):
      checks += 1
      Timeout.checkTimeout() // still within deadline
      Thread.sleep(60)
      checks += 1
      Timeout.checkTimeout() // expired
      checks += 1
      42
    assertEquals(r, None)
    assertEquals(checks, 2)
