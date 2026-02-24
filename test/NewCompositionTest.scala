package effect.tests

import effect.effects.*
import effect.effects.RefStore.*
import java.util.concurrent.atomic.AtomicInteger

class NewCompositionTest extends munit.FunSuite:

  // =====================================================
  // Async + State
  // =====================================================

  test("Async + State: forked tasks share state handler's mutable cell"):
    def program(using Async, State[Int]): Unit =
      val f1 = Async.fork { State.modify[Int](_ + 10); Integer.valueOf(0) }
      val f2 = Async.fork { State.modify[Int](_ + 20); Integer.valueOf(0) }
      f1.join()
      f2.join()
      ()

    val (state, _) = State.handler(0)(Async.handler(program))
    assertEquals(state, 30)

  // =====================================================
  // Async + Raise
  // =====================================================

  test("Async + Raise: error in fork propagates"):
    def program(using Async, Raise[EffectError]): String =
      val f = Async.fork { Raise.raise(EffectError("async error")); "never" }
      f.join()

    val result = Raise.handler(Async.handler(program))
    assertEquals(result, Left(EffectError("async error")))

  test("Async + Raise: successful fork with error handler"):
    def program(using Async, Raise[EffectError]): String =
      val f = Async.fork("success")
      f.join()

    val result = Raise.handler(Async.handler(program))
    assertEquals(result, Right("success"))

  // =====================================================
  // Async + Writer
  // =====================================================

  test("Async + Writer: forked tasks write to shared log"):
    def program(using Async, Writer[String]): Unit =
      val f1 = Async.fork { Writer.tell("from fork 1"); Integer.valueOf(0) }
      val f2 = Async.fork { Writer.tell("from fork 2"); Integer.valueOf(0) }
      f1.join()
      f2.join()
      ()

    val (log, _) = Writer.handler(Async.handler(program))
    assertEquals(log.toSet, Set("from fork 1", "from fork 2"))

  // =====================================================
  // Async + Reader
  // =====================================================

  test("Async + Reader: forked tasks can read environment"):
    def program(using Async, Reader[Int]): Integer =
      val f = Async.fork(Integer.valueOf(Reader.ask[Int]))
      f.join()

    val result = Reader.handler(42)(Async.handler(program))
    assertEquals(result, Integer.valueOf(42))

  // =====================================================
  // Async + Console
  // =====================================================

  test("Async + Console: forked task can use console"):
    def program(using Async, Console): Unit =
      val f = Async.fork { Console.printLine("from async"); Integer.valueOf(0) }
      f.join()
      ()

    val (output, _) = Console.testHandler(Nil)(Async.handler(program))
    assertEquals(output, List("from async"))

  // =====================================================
  // RefStore + State
  // =====================================================

  test("RefStore + State: refs and state coexist"):
    def program(using RefStore, State[Int]): (String, Int) =
      val r = RefStore.make("hello")
      State.modify[Int](_ + 1)
      r.set("world")
      State.modify[Int](_ + 1)
      (r.get, State.get[Int])

    val (state, result) = State.handler(0)(RefStore.handler(program))
    assertEquals(state, 2)
    assertEquals(result, ("world", 2))

  // =====================================================
  // RefStore + Raise
  // =====================================================

  test("RefStore + Raise: refs survive error"):
    RefStore.handler:
      val r = RefStore.make("initial")
      val result = Raise.handler[EffectError, String]:
        r.set("modified")
        Raise.raise(EffectError("error"))
      assertEquals(result, Left(EffectError("error")))
      assertEquals(r.get, "modified")

  // =====================================================
  // RefStore + Writer
  // =====================================================

  test("RefStore + Writer: log ref operations"):
    def program(using RefStore, Writer[String]): Unit =
      val counter = RefStore.make(Integer.valueOf(0))
      for i <- 1 to 3 do
        counter.modify(n => Integer.valueOf(n.intValue + 1))
        Writer.tell(s"counter=${counter.get}")

    val (log, _) = Writer.handler(RefStore.handler(program))
    assertEquals(log, List("counter=1", "counter=2", "counter=3"))

  // =====================================================
  // Timeout + State
  // =====================================================

  test("Timeout + State: state preserved on timeout"):
    def program(using State[Int], Timeout): Unit =
      State.modify[Int](_ + 1)
      State.modify[Int](_ + 1)
      Thread.sleep(50)
      Timeout.checkTimeout()
      State.modify[Int](_ + 1)

    val (state, result) =
      State.handler(0)(Timeout.handler(java.time.Duration.ofMillis(10))(program))
    assertEquals(state, 2)
    assertEquals(result, None)

  // =====================================================
  // Timeout + Raise
  // =====================================================

  test("Timeout + Raise: timeout or error, whichever comes first"):
    def program(using Raise[EffectError]): Int =
      Raise.raise(EffectError("fast error"))

    val result = Timeout.handler(java.time.Duration.ofSeconds(10))(Raise.handler(program))
    assertEquals(result, Some(Left(EffectError("fast error"))))

  // =====================================================
  // Amb + Writer
  // =====================================================

  test("Amb + Writer: each branch logs independently"):
    def program(using Amb, Writer[String]): Int =
      val x = Amb.choose(List(1, 2))
      Writer.tell(s"chose $x")
      x * 10

    val results = Amb.handler(Writer.handler(program))
    assertEquals(
      results.map(r => (r.output, r.result)).toSet,
      Set((List("chose 1"), 10), (List("chose 2"), 20))
    )

  // =====================================================
  // Amb + Raise
  // =====================================================

  test("Amb + Raise: errors prune branches with recovery"):
    def program(using Amb): Int =
      val x = Amb.choose(List(1, 2, 3, 4))
      Raise.catchError[EffectError, Int] {
        if x % 2 == 0 then Raise.raise(EffectError("even"))
        x * 10
      } { _ => -1 }

    val results = Amb.handler(program)
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
            synchronized:
              counter.modify(n => Integer.valueOf(n.intValue + 1))
            Integer.valueOf(0)
        forks.foreach(_.join())
      assertEquals(counter.get, Integer.valueOf(10))

  // =====================================================
  // Three+ new effects composed
  // =====================================================

  test("Async + Writer + State: parallel stateful logging"):
    def program(using Async, Writer[String], State[Int]): Unit =
      val f1 = Async.fork {
        State.modify[Int](_ + 1); Writer.tell("task 1 done"); Integer.valueOf(0)
      }
      val f2 = Async.fork {
        State.modify[Int](_ + 1); Writer.tell("task 2 done"); Integer.valueOf(0)
      }
      f1.join()
      f2.join()
      ()

    val (log, (state, _)) = Writer.handler(State.handler(0)(Async.handler(program)))
    assertEquals(state, 2)
    assertEquals(log.toSet, Set("task 1 done", "task 2 done"))

  test("RefStore + Reader + Writer: dependency-injected ref operations"):
    def program(using RefStore, Reader[String], Writer[String]): Unit =
      val r = RefStore.make("value")
      val prefix = Reader.ask[String]
      Writer.tell(s"$prefix: created ref with ${r.get}")
      r.set("updated")
      Writer.tell(s"$prefix: ref is now ${r.get}")

    val (log, _) = Writer.handler(Reader.handler("prefix")(RefStore.handler(program)))
    assertEquals(log, List("prefix: created ref with value", "prefix: ref is now updated"))

  test("Timeout + Writer + State: timeout with logging and state"):
    def program(using Timeout, Writer[String], State[Int]): Int =
      Writer.tell("start")
      State.modify[Int](_ + 1)
      Writer.tell(s"step ${State.get[Int]}")
      State.modify[Int](_ + 1)
      Writer.tell(s"step ${State.get[Int]}")
      State.get[Int]

    val (log, (state, result)) =
      Writer.handler(State.handler(0)(Timeout.handler(java.time.Duration.ofMillis(50))(program)))
    assertEquals(state, 2)
    assert(result.contains(2))
    assert(log.contains("start"))
    assert(log.contains("step 1"))
    assert(log.contains("step 2"))

  // =====================================================
  // Four new+old effects composed
  // =====================================================

  test("Async + Reader + State + Writer: full pipeline"):
    def program(using Async, Reader[String], State[Int], Writer[String]): Unit =
      val cfg = Reader.ask[String]
      Writer.tell(s"config=$cfg")
      val f = Async.fork {
        State.modify[Int](_ + 42); Writer.tell("async work done"); Integer.valueOf(0)
      }
      f.join()
      ()

    val (log, (state, _)) =
      Writer.handler(State.handler(0)(Reader.handler("cfg")(Async.handler(program))))
    assertEquals(state, 42)
    assert(log.contains("config=cfg"))
    assert(log.contains("async work done"))

  test("RefStore + Emit + State + Raise: full pipeline with dynamic refs"):
    def program(using RefStore, Emit[String], State[Int], Raise[ValidationError]): Unit =
      val items = RefStore.make(java.util.ArrayList[String]())
      for word <- List("hello", "world", "", "scala") do
        if word.isEmpty then Raise.raise(ValidationError("empty word"))
        State.modify[Int](_ + 1)
        val list = items.get
        list.add(word)
        items.set(list)
        Emit.emit(s"${State.get[Int]}: $word")

    val (emitted, (state, result)) =
      Emit.toList(State.handler(0)(Raise.handler(RefStore.handler(program))))
    assertEquals(emitted, List("1: hello", "2: world"))
    assertEquals(state, 2)
    assertEquals(result, Left(ValidationError("empty word")))
