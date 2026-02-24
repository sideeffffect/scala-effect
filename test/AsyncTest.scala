package effect.tests

import effect.effects.Async
import java.util.concurrent.atomic.AtomicInteger

class AsyncTest extends munit.FunSuite:

  test("fork and join single task"):
    val result = Async.handler:
      val f = Async.fork("hello")
      f.join()
    assertEquals(result, "hello")

  test("fork two tasks and join both"):
    val result = Async.handler:
      val f1 = Async.fork("a")
      val f2 = Async.fork("b")
      (f1.join(), f2.join())
    assertEquals(result, ("a", "b"))

  test("par runs concurrently and returns both results"):
    val result = Async.handler:
      Async.par(
        { Thread.sleep(50); "fast" },
        { Thread.sleep(50); "also fast" }
      )
    assertEquals(result, ("fast", "also fast"))

  test("par with computation"):
    val result = Async.handler:
      Async.par(
        { Thread.sleep(10); Integer.valueOf(21) },
        { Thread.sleep(10); Integer.valueOf(21) }
      )
    assertEquals(result, (Integer.valueOf(21), Integer.valueOf(21)))

  test("forked tasks run on virtual threads"):
    val result = Async.handler:
      val f = Async.fork(Thread.currentThread().isVirtual())
      java.lang.Boolean.valueOf(f.join())
    assertEquals(result, java.lang.Boolean.TRUE)

  test("fork many tasks"):
    val counter = AtomicInteger(0)
    Async.handler:
      val forks = (1 to 100).map: i =>
        Async.fork:
          counter.incrementAndGet()
          Integer.valueOf(i)
      forks.foreach(_.join())
    assertEquals(counter.get(), 100)

  test("handler propagates exceptions"):
    val result = Async.handlerEither:
      val f = Async.fork:
        throw RuntimeException("boom")
        "never"
      f.join()
    assert(result.isLeft, s"Expected Left but got $result")
    result match
      case Left(e: RuntimeException) => assertEquals(e.getMessage, "boom")
      case Left(other)               =>
        assertEquals(other.getMessage, "boom") // any exception type
      case _ => fail("expected Left")

  test("race returns first result"):
    val result = Async.race(
      { Thread.sleep(500); "slow" },
      { Thread.sleep(10); "fast" }
    )
    assertEquals(result, "fast")

  test("par with side effects"):
    val counter = AtomicInteger(0)
    Async.handler:
      val (a, b) = Async.par(
        { counter.incrementAndGet(); "x" },
        { counter.incrementAndGet(); "y" }
      )
      (a, b)
    assertEquals(counter.get(), 2)

  test("nested async handlers"):
    val result = Async.handler:
      val outer = Async.fork:
        Async.handler:
          val inner = Async.fork("inner")
          inner.join()
      outer.join()
    assertEquals(result, "inner")
