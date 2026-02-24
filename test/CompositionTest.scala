package effect.tests

import effect.effects.*

class CompositionTest extends munit.FunSuite:

  // =====================================================
  // State + Raise: handler ordering determines semantics
  // =====================================================

  test("State outside Raise = global state (state survives errors)"):
    def program(using State[Int], Raise[EffectError]): Int =
      State.modify[Int](_ + 1)
      State.modify[Int](_ + 1)
      Raise.raise(EffectError("boom"))

    val (state, result) = State.handler(0)(Raise.handler(program))
    assertEquals(state, 2)
    assertEquals(result, Left(EffectError("boom")))

  test("Raise outside State = local state (state lost on error)"):
    def program(using State[Int], Raise[EffectError]): Int =
      State.modify[Int](_ + 1)
      State.modify[Int](_ + 1)
      Raise.raise(EffectError("boom"))

    val result = Raise.handler(State.handler(0)(program))
    assertEquals(result, Left(EffectError("boom")))

  test("State + Raise: successful path preserves state in both orderings"):
    def program(using State[Int], Raise[EffectError]): Int =
      State.modify[Int](_ + 1)
      42

    val (s1, r1) = State.handler(0)(Raise.handler(program))
    assertEquals(s1, 1)
    assertEquals(r1, Right(42))

    val r2 = Raise.handler(State.handler(0)(program))
    assertEquals(r2, Right((1, 42)))

  // =====================================================
  // State + Reader
  // =====================================================

  test("State + Reader: reader provides config, state accumulates"):
    def program(using State[Int], Reader[Int]): Int =
      val factor = Reader.ask[Int]
      State.modify[Int](_ + factor)
      State.modify[Int](_ + factor)
      State.get[Int]

    val (state, result) = State.handler(0)(Reader.handler(10)(program))
    assertEquals(state, 20)
    assertEquals(result, 20)

  test("Reader local does not affect state"):
    def program(using State[Int], Reader[Int]): Int =
      State.modify[Int](_ + Reader.ask[Int])
      Reader.local[Int, Unit](_ * 100):
        State.modify[Int](_ + Reader.ask[Int])
      State.modify[Int](_ + Reader.ask[Int])
      State.get[Int]

    val (state, result) = State.handler(0)(Reader.handler(1)(program))
    assertEquals(state, 102)
    assertEquals(result, 102)

  // =====================================================
  // State + Writer
  // =====================================================

  test("State + Writer: log state transitions"):
    def program(using State[Int], Writer[String]): Unit =
      Writer.tell(s"start: ${State.get[Int]}")
      State.modify[Int](_ + 10)
      Writer.tell(s"after +10: ${State.get[Int]}")
      State.modify[Int](_ * 2)
      Writer.tell(s"after *2: ${State.get[Int]}")

    val (log, (state, _)) = Writer.handler(State.handler(0)(program))
    assertEquals(log, List("start: 0", "after +10: 10", "after *2: 20"))
    assertEquals(state, 20)

  test("Writer censor interacts correctly with state"):
    def program(using State[Int], Writer[String]): Unit =
      Writer.tell("before")
      Writer.censor[String, Unit](_.toUpperCase):
        State.modify[Int](_ + 1)
        Writer.tell(s"counter=${State.get[Int]}")
      Writer.tell("after")

    val (log, (state, _)) = Writer.handler(State.handler(0)(program))
    assertEquals(log, List("before", "COUNTER=1", "after"))
    assertEquals(state, 1)

  // =====================================================
  // State + Emit
  // =====================================================

  test("State + Emit: numbered emissions"):
    def program(using State[Int], Emit[String]): Unit =
      for item <- List("a", "b", "c") do
        State.modify[Int](_ + 1)
        Emit.emit(s"${State.get[Int]}. $item")

    val (emitted, (count, _)) = Emit.toList(State.handler(0)(program))
    assertEquals(emitted, List("1. a", "2. b", "3. c"))
    assertEquals(count, 3)

  // =====================================================
  // Reader + Writer
  // =====================================================

  test("Reader + Writer: log depends on environment"):
    def program(using Reader[String], Writer[String]): Unit =
      val env = Reader.ask[String]
      Writer.tell(s"[$env] Starting")
      Writer.tell(s"[$env] Done")

    val (log, _) = Writer.handler(Reader.handler("PROD")(program))
    assertEquals(log, List("[PROD] Starting", "[PROD] Done"))

  test("Reader local + Writer censor"):
    def program(using Reader[String], Writer[String]): Unit =
      Writer.tell(s"mode=${Reader.ask[String]}")
      Reader.local[String, Unit](_ => "debug"):
        Writer.censor[String, Unit](s => s"[DEBUG] $s"):
          Writer.tell(s"mode=${Reader.ask[String]}")
      Writer.tell(s"mode=${Reader.ask[String]}")

    val (log, _) = Writer.handler(Reader.handler("normal")(program))
    assertEquals(log, List("mode=normal", "[DEBUG] mode=debug", "mode=normal"))

  // =====================================================
  // Reader + Raise
  // =====================================================

  test("Reader + Raise: environment-dependent errors"):
    def validate(using Reader[Int], Raise[ValidationError]): Int =
      val limit = Reader.ask[Int]
      if limit <= 0 then Raise.raise(ValidationError("limit must be positive"))
      else limit

    val r1 = Reader.handler(10)(Raise.handler(validate))
    assertEquals(r1, Right(10))

    val r2 = Reader.handler(-1)(Raise.handler(validate))
    assertEquals(r2, Left(ValidationError("limit must be positive")))

  // =====================================================
  // Writer + Raise
  // =====================================================

  test("Writer outside Raise = log survives errors"):
    def program(using Writer[String], Raise[EffectError]): Int =
      Writer.tell("step 1")
      Writer.tell("step 2")
      Raise.raise(EffectError("boom"))

    val (log, result) = Writer.handler(Raise.handler(program))
    assertEquals(log, List("step 1", "step 2"))
    assertEquals(result, Left(EffectError("boom")))

  test("Raise outside Writer = log lost on error"):
    def program(using Writer[String], Raise[EffectError]): Int =
      Writer.tell("step 1")
      Writer.tell("step 2")
      Raise.raise(EffectError("boom"))

    val result = Raise.handler(Writer.handler(program))
    assertEquals(result, Left(EffectError("boom")))

  // =====================================================
  // Console + State
  // =====================================================

  test("Console + State: interactive counter"):
    def program(using Console, State[Int]): Unit =
      var running = true
      while running do
        val cmd = Console.readLine()
        cmd match
          case "inc"  => State.modify[Int](_ + 1)
          case "show" => Console.printLine(s"count=${State.get[Int]}")
          case "quit" => running = false
          case _      => Console.printLine("unknown")

    val (output, (count, _)) =
      Console.testHandler(List("inc", "inc", "inc", "show", "quit"))(State.handler(0)(program))
    assertEquals(count, 3)
    assertEquals(output, List("count=3"))

  // =====================================================
  // Console + Raise
  // =====================================================

  test("Console + Raise: input validation"):
    def program(using Console, Raise[ParseError]): Int =
      Console.printLine("Enter number:")
      val input = Console.readLine()
      input.toIntOption match
        case Some(n) => n
        case None    => Raise.raise(ParseError(s"Invalid: $input"))

    val (output, result) =
      Console.testHandler(List("not-a-number"))(Raise.handler(program))
    assertEquals(output, List("Enter number:"))
    assertEquals(result, Left(ParseError("Invalid: not-a-number")))

  // =====================================================
  // Emit + Raise
  // =====================================================

  test("Emit outside Raise = emissions survive error"):
    def program(using Emit[Int], Raise[EffectError]): Int =
      Emit.emit(1)
      Emit.emit(2)
      Raise.raise(EffectError("stop"))

    val (emitted, result) = Emit.toList(Raise.handler(program))
    assertEquals(emitted, List(1, 2))
    assertEquals(result, Left(EffectError("stop")))

  test("Raise outside Emit = emissions lost on error"):
    def program(using Emit[Int], Raise[EffectError]): Int =
      Emit.emit(1)
      Emit.emit(2)
      Raise.raise(EffectError("stop"))

    val result = Raise.handler(Emit.toList(program))
    assertEquals(result, Left(EffectError("stop")))

  // =====================================================
  // Nondet + State
  // =====================================================

  test("Nondet + State: each branch gets fresh state"):
    def program(using Nondet): Int =
      val (state, _) = State.handler(0):
        val x = Nondet.choose(List(1, 2, 3))
        State.modify[Int](_ + x)
      state

    val results = Nondet.handler(program)
    assertEquals(results, List(1, 2, 3))

  // =====================================================
  // Nondet + Raise
  // =====================================================

  test("Nondet + Raise: errors prune branches"):
    def program(using Nondet): Int =
      val x = Nondet.choose(List(1, 2, 3, 4, 5))
      Raise.catchError[EffectError, Int] {
        if x % 2 == 0 then Raise.raise(EffectError("even"))
        x
      } { _ => -1 }

    val results = Nondet.handler(program)
    assertEquals(results, List(1, -1, 3, -1, 5))

  // =====================================================
  // Three effects composed
  // =====================================================

  test("Reader + State + Writer: config-driven stateful logging"):
    def program(using Reader[Int], State[Int], Writer[String]): List[Int] =
      val multiplier = Reader.ask[Int]
      List(1, 2, 3).map: item =>
        val value = item * multiplier
        State.modify[Int](_ + value)
        Writer.tell(s"$item * $multiplier = $value (total=${State.get[Int]})")
        value

    val (log, (state, result)) =
      Writer.handler(State.handler(0)(Reader.handler(3)(program)))
    assertEquals(result, List(3, 6, 9))
    assertEquals(state, 18)
    assertEquals(log.length, 3)
    assert(log.head.contains("1 * 3 = 3"))
    assert(log.last.contains("total=18"))

  test("Reader + Writer + Raise: environment-dependent logging with errors"):
    def program(using Reader[Int], Writer[String], Raise[LimitExceeded]): Int =
      val threshold = Reader.ask[Int]
      for i <- 1 to 10 do
        Writer.tell(s"processing $i")
        if i > threshold then Raise.raise(LimitExceeded(s"exceeded threshold $threshold at $i"))
      42

    val (log, result) =
      Writer.handler(Reader.handler(5)(Raise.handler(program)))
    assertEquals(log.length, 6)
    assertEquals(result, Left(LimitExceeded("exceeded threshold 5 at 6")))

  test("State + Writer + Raise: stateful logging with error recovery"):
    def program(using State[Int], Writer[String]): Int =
      val items = List(1, -1, 2, -2, 3)
      for item <- items do
        Writer.tell(s"processing $item")
        val processed = Raise.catchError[ValidationError, Int] {
          if item < 0 then Raise.raise(ValidationError(s"negative: $item"))
          item
        } { e =>
          Writer.tell(s"  recovered from: ${e.msg}")
          0
        }
        State.modify[Int](_ + processed)
      State.get[Int]

    val (log, (state, result)) =
      Writer.handler(State.handler(0)(program))
    assertEquals(state, 6)
    assertEquals(result, 6)
    assert(log.exists(_.contains("recovered from: negative: -1")))
    assert(log.exists(_.contains("recovered from: negative: -2")))

  // =====================================================
  // Four effects composed
  // =====================================================

  test("Reader + State + Writer + Console: full application"):
    def program(using Reader[String], State[Int], Writer[String], Console): Unit =
      val appName = Reader.ask[String]
      Console.printLine(s"Welcome to $appName")
      Console.printLine("Enter name:")
      val name = Console.readLine()
      Writer.tell(s"User: $name")
      Console.printLine("How many items?")
      val count = Console.readLine().toInt
      for i <- 1 to count do
        State.modify[Int](_ + 1)
        Writer.tell(s"Item $i for $name")

    val (output, (log, (state, _))) =
      Console.testHandler(List("alice", "3")):
        Writer.handler(State.handler(0)(Reader.handler("app")(program)))
    assertEquals(output, List("Welcome to app", "Enter name:", "How many items?"))
    assertEquals(
      log,
      List("User: alice", "Item 1 for alice", "Item 2 for alice", "Item 3 for alice")
    )
    assertEquals(state, 3)

  test("Reader + State + Writer + Raise: pipeline with error handling"):
    def pipeline(using Reader[Int], State[Int], Writer[String], Raise[ValidationError]): List[Int] =
      val factor = Reader.ask[Int]
      List(10, 20, 0, 30).map: item =>
        State.modify[Int](_ + 1)
        Writer.tell(s"step ${State.get[Int]}: processing $item")
        if item == 0 then Raise.raise(ValidationError("zero encountered"))
        item * factor

    val (log, (state, result)) =
      Writer.handler(State.handler(0)(Reader.handler(2)(Raise.handler(pipeline))))
    assertEquals(state, 3)
    assertEquals(result, Left(ValidationError("zero encountered")))
    assertEquals(log.length, 3)

  // =====================================================
  // Five effects composed
  // =====================================================

  test("Reader + State + Writer + Emit + Raise: kitchen sink"):
    def program(using
        Reader[Int],
        State[Int],
        Writer[String],
        Emit[Int],
        Raise[LimitExceeded]
    ): Unit =
      val base = Reader.ask[Int]
      for i <- 1 to 5 do
        State.modify[Int](_ + 1)
        val n = State.get[Int]
        Writer.tell(s"iteration $n")
        Emit.emit(base + n)
        if n == 3 then Raise.raise(LimitExceeded("stopped at 3"))

    val (emitted, (log, (state, result))) =
      Emit.toList(Writer.handler(State.handler(0)(Reader.handler(100)(Raise.handler(program)))))
    assertEquals(emitted, List(101, 102, 103))
    assertEquals(log, List("iteration 1", "iteration 2", "iteration 3"))
    assertEquals(state, 3)
    assertEquals(result, Left(LimitExceeded("stopped at 3")))

  // =====================================================
  // Nondet + Writer: nondeterministic logging
  // =====================================================

  test("Nondet + Writer: each branch logs independently"):
    def program(using Nondet, Writer[String]): String =
      val x = Nondet.choose(List("a", "b"))
      Writer.tell(s"chose: $x")
      x.toUpperCase

    val results = Nondet.handler(Writer.handler(program))
    assertEquals(results.length, 2)
    assertEquals(results(0), (List("chose: a"), "A"))
    assertEquals(results(1), (List("chose: b"), "B"))

  test("Nondet + Reader: shared config across branches"):
    def program(using Nondet, Reader[Int]): Int =
      val x = Nondet.choose(List(1, 2, 3))
      x * Reader.ask[Int]

    val results = Nondet.handler(Reader.handler(10)(program))
    assertEquals(results, List(10, 20, 30))

  test("Emit + Writer: structured and unstructured output"):
    def program(using Emit[Int], Writer[String]): Unit =
      Writer.tell("starting")
      Emit.emit(1)
      Writer.tell("middle")
      Emit.emit(2)
      Writer.tell("done")
      Emit.emit(3)

    val (emitted, (log, _)) = Emit.toList(Writer.handler(program))
    assertEquals(emitted, List(1, 2, 3))
    assertEquals(log, List("starting", "middle", "done"))

  test("deeply nested State handlers"):
    def program(using State[Int]): Int =
      State.modify[Int](_ + 1)
      State.get[Int]

    val (s1, (s2, (s3, v))) =
      State.handler(0)(State.handler(100)(State.handler(1000)(program)))
    assertEquals(s1, 0)
    assertEquals(s2, 100)
    assertEquals(s3, 1001)
    assertEquals(v, 1001)

  test("deeply nested Reader handlers shadow correctly"):
    val v = Reader.handler(1):
      val a = Reader.ask[Int]
      val b = Reader.handler(2):
        val x = Reader.ask[Int]
        val y = Reader.handler(3)(Reader.ask[Int])
        (x, y)
      (a, b)
    assertEquals(v, (1, (2, 3)))

  test("Fail + State: simple failure preserves state"):
    def program(using State[Int], Fail): Int =
      State.modify[Int](_ + 1)
      State.modify[Int](_ + 1)
      Fail.fail()

    val (state, result) = State.handler(0)(Fail.handler(program))
    assertEquals(state, 2)
    assertEquals(result, None)

  test("nested Raise handlers catch at correct level"):
    def inner(using Raise[ValidationError]): String =
      Raise.raise(ValidationError("inner"))

    val result = Raise.handler[EffectError, String]:
      Raise.handler(inner) match
        case Left(e)  => s"inner caught: ${e.msg}"
        case Right(s) => s
    assertEquals(result, Right("inner caught: inner"))

  test("outer Raise catches when inner type doesn't match"):
    def program(using Raise[ValidationError], Raise[EffectError]): String =
      Raise.raise(EffectError("outer error"))

    val result = Raise.handler(Raise.handler[ValidationError, String](program))
    assertEquals(result, Left(EffectError("outer error")))

  test("Console + Writer + State: full interactive session"):
    def program(using Console, Writer[String], State[Int]): Unit =
      var running = true
      while running do
        val cmd = Console.readLine()
        Writer.tell(s"cmd: $cmd")
        if cmd.startsWith("add ") then
          val n = cmd.drop(4).toInt
          State.modify[Int](_ + n)
        else if cmd == "show" then Console.printLine(s"total=${State.get[Int]}")
        else if cmd == "quit" then running = false

    val (output, (log, (state, _))) =
      Console.testHandler(List("add 5", "add 3", "show", "quit")):
        Writer.handler(State.handler(0)(program))
    assertEquals(output, List("total=8"))
    assertEquals(state, 8)
    assertEquals(log, List("cmd: add 5", "cmd: add 3", "cmd: show", "cmd: quit"))

  test("Reader + Emit: emit values based on environment"):
    def program(using Reader[List[String]], Emit[String]): Unit =
      val items = Reader.ask[List[String]]
      items.foreach(item => Emit.emit(s"item: $item"))

    val (emitted, _) = Emit.toList(Reader.handler(List("x", "y", "z"))(program))
    assertEquals(emitted, List("item: x", "item: y", "item: z"))

  test("Nondet + Writer + State: per-branch logging and state"):
    def program(using Nondet, Writer[String], State[Int]): Int =
      val x = Nondet.choose(List(10, 20))
      State.modify[Int](_ + x)
      Writer.tell(s"chose $x, state=${State.get[Int]}")
      State.get[Int]

    val results = Nondet.handler(Writer.handler(State.handler(0)(program)))
    assertEquals(results.length, 2)
    assertEquals(results(0), (List("chose 10, state=10"), (10, 10)))
    assertEquals(results(1), (List("chose 20, state=20"), (20, 20)))

  test("Emit + State + Reader: data pipeline"):
    def program(using Emit[String], State[Int], Reader[String]): Unit =
      val prefix = Reader.ask[String]
      for word <- List("hello", "world", "scala") do
        State.modify[Int](_ + 1)
        Emit.emit(s"$prefix${State.get[Int]}: $word")

    val (emitted, (count, _)) =
      Emit.toList(State.handler(0)(Reader.handler("> ")(program)))
    assertEquals(count, 3)
    assertEquals(emitted, List("> 1: hello", "> 2: world", "> 3: scala"))

  test("six effects composed: Reader + State + Writer + Emit + Console + Raise"):
    def program(using
        Reader[String],
        State[Int],
        Writer[String],
        Emit[Int],
        Console,
        Raise[EffectError]
    ): Unit =
      Console.printLine(s"config=${Reader.ask[String]}")
      val cmd = Console.readLine()
      if cmd != "go" then Raise.raise(EffectError("expected 'go'"))
      for i <- 1 to 3 do
        State.modify[Int](_ + 1)
        Emit.emit(State.get[Int] * 10)
        Writer.tell(s"step $i done")

    val (output, (emitted, (log, (state, result)))) =
      Console.testHandler(List("go")):
        Emit.toList:
          Writer.handler:
            State.handler(0):
              Reader.handler("cfg")(Raise.handler(program))
    assertEquals(output, List("config=cfg"))
    assertEquals(emitted, List(10, 20, 30))
    assertEquals(log, List("step 1 done", "step 2 done", "step 3 done"))
    assertEquals(state, 3)
    assertEquals(result, Right(()))
