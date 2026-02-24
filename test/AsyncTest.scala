package effect.tests

import effect.effects.Async
import java.util.concurrent.atomic.AtomicInteger

class AsyncTest extends munit.FunSuite:

  test("fork and join single task"):
    def program(using Async): String =
      val f = Async.fork("hello")
      f.join()

    val result = Async.handler(program)
    assertEquals(result, "hello")

  test("fork two tasks and join both"):
    def program(using Async): (String, String) =
      val f1 = Async.fork("a")
      val f2 = Async.fork("b")
      (f1.join(), f2.join())

    val result = Async.handler(program)
    assertEquals(result, ("a", "b"))

  test("par runs concurrently and returns both results"):
    def program(using Async): (String, String) =
      Async.par(
        { Thread.sleep(50); "fast" },
        { Thread.sleep(50); "also fast" }
      )

    val result = Async.handler(program)
    assertEquals(result, ("fast", "also fast"))

  test("par with computation"):
    def program(using Async): (Integer, Integer) =
      Async.par(
        { Thread.sleep(10); Integer.valueOf(21) },
        { Thread.sleep(10); Integer.valueOf(21) }
      )

    val result = Async.handler(program)
    assertEquals(result, (Integer.valueOf(21), Integer.valueOf(21)))

  test("forked tasks run on virtual threads"):
    def program(using Async): java.lang.Boolean =
      val f = Async.fork(Thread.currentThread().isVirtual())
      java.lang.Boolean.valueOf(f.join())

    val result = Async.handler(program)
    assertEquals(result, java.lang.Boolean.TRUE)

  test("fork many tasks"):
    val counter = AtomicInteger(0)
    def program(using Async): Unit =
      val forks = (1 to 100).map: i =>
        Async.fork { counter.incrementAndGet(); Integer.valueOf(i) }
      forks.foreach(_.join())

    Async.handler(program)
    assertEquals(counter.get(), 100)

  test("handler propagates exceptions"):
    def program(using Async): String =
      val f = Async.fork { throw RuntimeException("boom"); "never" }
      f.join()

    val result = Async.handlerEither(program)
    assert(result.isLeft, s"Expected Left but got $result")
    result match
      case Left(e: RuntimeException) => assertEquals(e.getMessage, "boom")
      case Left(other)               => assertEquals(other.getMessage, "boom")
      case _                         => fail("expected Left")

  test("race returns first result"):
    val result = Async.race(
      { Thread.sleep(500); "slow" },
      { Thread.sleep(10); "fast" }
    )
    assertEquals(result, "fast")

  test("par with side effects"):
    val counter = AtomicInteger(0)
    def program(using Async): (String, String) =
      Async.par(
        { counter.incrementAndGet(); "x" },
        { counter.incrementAndGet(); "y" }
      )

    Async.handler(program)
    assertEquals(counter.get(), 2)

  test("nested async handlers"):
    def program(using Async): String =
      val outer = Async.fork:
        Async.handler:
          val inner = Async.fork("inner")
          inner.join()
      outer.join()

    val result = Async.handler(program)
    assertEquals(result, "inner")
