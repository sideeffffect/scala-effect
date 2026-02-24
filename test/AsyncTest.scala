package effect.tests

import effect.effects.Async
import java.util.concurrent.{atomic, CancellationException}
import atomic.{AtomicBoolean, AtomicInteger}

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

  // =====================================================
  // Cancellation tests
  // =====================================================

  test("cancel interrupts a sleeping task"):
    val started = AtomicBoolean(false)
    val interrupted = AtomicBoolean(false)

    def program(using Async): String =
      val f = Async.fork:
        started.set(true)
        try Thread.sleep(10_000)
        catch case _: InterruptedException => interrupted.set(true)
        "done"
      Thread.sleep(50) // let the fork start
      f.cancel()
      "cancelled"

    val result = Async.handler(program)
    assertEquals(result, "cancelled")
    assert(started.get(), "task should have started")
    assert(interrupted.get(), "task should have been interrupted")

  test("join after cancel throws CancellationException"):
    def program(using Async): String =
      val f = Async.fork:
        Thread.sleep(10_000)
        "never"
      Thread.sleep(50)
      f.cancel()
      try
        f.join()
        "should not reach"
      catch case _: CancellationException => "caught cancellation"

    val result = Async.handler(program)
    assertEquals(result, "caught cancellation")

  test("isCancelled reflects cancel state"):
    def program(using Async): (Boolean, Boolean) =
      val f = Async.fork:
        Thread.sleep(10_000)
        "never"
      Thread.sleep(50)
      val before = f.isCancelled
      f.cancel()
      val after = f.isCancelled
      (before, after)

    val result = Async.handler(program)
    assertEquals(result, (false, true))

  test("isDone reflects completion state"):
    def program(using Async): (Boolean, Boolean) =
      val f = Async.fork:
        Thread.sleep(10)
        "done"
      val before = f.isDone
      Thread.sleep(100)
      val after = f.isDone
      (before, after)

    val result = Async.handler(program)
    // before may or may not be done depending on scheduling, but after should be
    assertEquals(result._2, true)

  test("checkCancelled is no-op when not interrupted"):
    def program(using Async): String =
      Async.checkCancelled()
      "ok"

    val result = Async.handler(program)
    assertEquals(result, "ok")

  test("cancel does not affect other forks"):
    val completed = AtomicBoolean(false)

    def program(using Async): String =
      val f1 = Async.fork:
        Thread.sleep(10_000)
        "slow"
      val f2 = Async.fork:
        Thread.sleep(50)
        completed.set(true)
        "fast"
      Thread.sleep(20)
      f1.cancel()
      f2.join()

    val result = Async.handler(program)
    assertEquals(result, "fast")
    assert(completed.get(), "f2 should have completed")

  test("cancel already-completed task is no-op"):
    def program(using Async): String =
      val f = Async.fork("instant")
      Thread.sleep(50) // let it complete
      f.cancel() // should be no-op
      f.join()

    val result = Async.handler(program)
    assertEquals(result, "instant")
