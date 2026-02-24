package effect.tests

import effect.effects.*
import java.util.concurrent.atomic.AtomicInteger

class NewCompositionTest extends munit.FunSuite:

  // =====================================================
  // Async + State
  // =====================================================

  test("Async + State: forked tasks share state handler's mutable cell"):
    val (state, _) = State.handler(0):
      Async.handler:
        val f1 = Async.fork:
          State.modify[Int](_ + 10)
          Integer.valueOf(0)
        val f2 = Async.fork:
          State.modify[Int](_ + 20)
          Integer.valueOf(0)
        f1.join()
        f2.join()
    assertEquals(state, 30)

  // =====================================================
  // Async + Raise
  // =====================================================

  test("Async + Raise: error in fork propagates"):
    // Raise.raise throws a RaiseException. Inside a fork, this propagates as
    // a regular exception through the CompletableFuture. The outer Raise.handler
    // catches it because the token matches.
    val result = Raise.handler[String, String]:
      Async.handler:
        val f = Async.fork:
          Raise.raise[String]("async error")
          "never"
        f.join()
    assertEquals(result, Left("async error"))

  test("Async + Raise: successful fork with error handler"):
    val result = Raise.handler[String, String]:
      Async.handler:
        val f = Async.fork("success")
        f.join()
    assertEquals(result, Right("success"))

  // =====================================================
  // Async + Writer
  // =====================================================

  test("Async + Writer: forked tasks write to shared log"):
    val (log, _) = Writer.handler[String, Unit]:
      Async.handler:
        val f1 = Async.fork:
          Writer.tell("from fork 1")
          Integer.valueOf(0)
        val f2 = Async.fork:
          Writer.tell("from fork 2")
          Integer.valueOf(0)
        f1.join()
        f2.join()
    assertEquals(log.toSet, Set("from fork 1", "from fork 2"))

  // =====================================================
  // Async + Reader
  // =====================================================

  test("Async + Reader: forked tasks can read environment"):
    val result = Reader.handler(42):
      Async.handler:
        val f = Async.fork(Integer.valueOf(Reader.ask[Int]))
        f.join()
    assertEquals(result, Integer.valueOf(42))

  // =====================================================
  // Async + Console
  // =====================================================

  test("Async + Console: forked task can use console"):
    val (output, _) = Console.testHandler(Nil):
      Async.handler:
        val f = Async.fork:
          Console.printLine("from async")
          Integer.valueOf(0)
        f.join()
    assertEquals(output, List("from async"))

  // =====================================================
  // RefStore + State
  // =====================================================

  test("RefStore + State: refs and state coexist"):
    val (state, result) = State.handler(0):
      RefStore.handler:
        val r = RefStore.make("hello")
        State.modify[Int](_ + 1)
        RefStore.set(r, "world")
        State.modify[Int](_ + 1)
        (RefStore.get(r), State.get[Int])
    assertEquals(state, 2)
    assertEquals(result, ("world", 2))

  // =====================================================
  // RefStore + Raise
  // =====================================================

  test("RefStore + Raise: refs survive error"):
    RefStore.handler:
      val r = RefStore.make("initial")
      val result = Raise.handler[String, String]:
        RefStore.set(r, "modified")
        Raise.raise("error")
      assertEquals(result, Left("error"))
      assertEquals(RefStore.get(r), "modified")

  // =====================================================
  // RefStore + Writer
  // =====================================================

  test("RefStore + Writer: log ref operations"):
    val (log, _) = Writer.handler[String, Unit]:
      RefStore.handler:
        val counter = RefStore.make(Integer.valueOf(0))
        for i <- 1 to 3 do
          RefStore.modify(counter)(n => Integer.valueOf(n.intValue + 1))
          Writer.tell(s"counter=${RefStore.get(counter)}")
    assertEquals(log, List("counter=1", "counter=2", "counter=3"))

  // =====================================================
  // Timeout + State
  // =====================================================

  test("Timeout + State: state preserved on timeout"):
    val (state, result) = State.handler(0):
      Timeout.handler(java.time.Duration.ofMillis(10)):
        State.modify[Int](_ + 1)
        State.modify[Int](_ + 1)
        Thread.sleep(50)
        Timeout.checkTimeout()
        State.modify[Int](_ + 1) // should not execute
    assertEquals(state, 2) // first two modifications preserved
    assertEquals(result, None)

  // =====================================================
  // Timeout + Raise
  // =====================================================

  test("Timeout + Raise: timeout or error, whichever comes first"):
    val result = Timeout.handler(java.time.Duration.ofSeconds(10)):
      Raise.handler[String, Int]:
        Raise.raise("fast error") // error before timeout
    assertEquals(result, Some(Left("fast error")))

  // =====================================================
  // Amb + Writer
  // =====================================================

  test("Amb + Writer: each branch logs independently"):
    val results = Amb.handler[(List[String], Int)]:
      Writer.handler[String, Int]:
        val x = Amb.choose(List(1, 2))
        Writer.tell(s"chose $x")
        x * 10
    assertEquals(results.toSet, Set((List("chose 1"), 10), (List("chose 2"), 20)))

  // =====================================================
  // Amb + Raise
  // =====================================================

  test("Amb + Raise: errors prune branches with recovery"):
    val results = Amb.handler[Int]:
      val x = Amb.choose(List(1, 2, 3, 4))
      Raise.catchError[String, Int] {
        if x % 2 == 0 then Raise.raise("even")
        x * 10
      } { _ => -1 }
    assertEquals(results.toSet, Set(10, -1, 30, -1))

  // =====================================================
  // RefStore + Async: concurrent ref access
  // =====================================================

  test("RefStore + Async: concurrent updates"):
    RefStore.handler:
      val counter = RefStore.make(Integer.valueOf(0))
      Async.handler:
        val forks = (1 to 10).map: _ =>
          Async.fork:
            // Each fork increments; not atomic but test is sequential enough
            synchronized:
              RefStore.modify(counter)(n => Integer.valueOf(n.intValue + 1))
            Integer.valueOf(0)
        forks.foreach(_.join())
      assertEquals(RefStore.get(counter), Integer.valueOf(10))

  // =====================================================
  // Three+ new effects composed
  // =====================================================

  test("Async + Writer + State: parallel stateful logging"):
    val (log, (state, _)) = Writer.handler[String, (Int, Unit)]:
      State.handler(0):
        Async.handler:
          val f1 = Async.fork:
            State.modify[Int](_ + 1)
            Writer.tell("task 1 done")
            Integer.valueOf(0)
          val f2 = Async.fork:
            State.modify[Int](_ + 1)
            Writer.tell("task 2 done")
            Integer.valueOf(0)
          f1.join()
          f2.join()
    assertEquals(state, 2)
    assertEquals(log.toSet, Set("task 1 done", "task 2 done"))

  test("RefStore + Reader + Writer: dependency-injected ref operations"):
    val (log, _) = Writer.handler[String, Unit]:
      Reader.handler("prefix"):
        RefStore.handler:
          val r = RefStore.make("value")
          val prefix = Reader.ask[String]
          Writer.tell(s"$prefix: created ref with ${RefStore.get(r)}")
          RefStore.set(r, "updated")
          Writer.tell(s"$prefix: ref is now ${RefStore.get(r)}")
    assertEquals(log, List("prefix: created ref with value", "prefix: ref is now updated"))

  test("Timeout + Writer + State: timeout with logging and state"):
    val (log, (state, result)) =
      Writer.handler[String, (Int, Option[Int])]:
        State.handler(0):
          Timeout.handler(java.time.Duration.ofMillis(50)):
            Writer.tell("start")
            State.modify[Int](_ + 1)
            Writer.tell(s"step ${State.get[Int]}")
            State.modify[Int](_ + 1)
            Writer.tell(s"step ${State.get[Int]}")
            State.get[Int]
    assertEquals(state, 2)
    assert(result.contains(2))
    assert(log.contains("start"))
    assert(log.contains("step 1"))
    assert(log.contains("step 2"))

  // =====================================================
  // Four new+old effects composed
  // =====================================================

  test("Async + Reader + State + Writer: full pipeline"):
    val (log, (state, _)) = Writer.handler[String, (Int, Unit)]:
      State.handler(0):
        Reader.handler("cfg"):
          Async.handler:
            val cfg = Reader.ask[String]
            Writer.tell(s"config=$cfg")
            val f = Async.fork:
              State.modify[Int](_ + 42)
              Writer.tell("async work done")
              Integer.valueOf(0)
            f.join()
    assertEquals(state, 42)
    assert(log.contains("config=cfg"))
    assert(log.contains("async work done"))

  test("RefStore + Emit + State + Raise: full pipeline with dynamic refs"):
    val (emitted, (state, result)) =
      Emit.toList[String, (Int, Either[String, Unit])]:
        State.handler(0):
          Raise.handler[String, Unit]:
            RefStore.handler:
              val items = RefStore.make(java.util.ArrayList[String]())
              for word <- List("hello", "world", "", "scala") do
                if word.isEmpty then Raise.raise("empty word")
                State.modify[Int](_ + 1)
                val list = RefStore.get(items)
                list.add(word)
                RefStore.set(items, list)
                Emit.emit(s"${State.get[Int]}: $word")
    assertEquals(emitted, List("1: hello", "2: world"))
    assertEquals(state, 2)
    assertEquals(result, Left("empty word"))
