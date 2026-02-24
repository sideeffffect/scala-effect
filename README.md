# scala-effect

A direct-style algebraic effect system for Scala 3, inspired by the Haskell
[Effective](https://github.com/zenzike/effective) library.

Uses **Scala 3 capabilities** (context functions, `caps.SharedCapability`,
capture checking) and **JDK 25 structured concurrency** (virtual threads,
`StructuredTaskScope`). All effects are tracked in types and capabilities
cannot escape their handler scope — enforced at compile time.

## Quick start

Requires [scala-cli](https://scala-cli.virtuslab.org/) >= 1.12.

```bash
scala-cli compile .       # compile (downloads JDK 25 automatically)
scala-cli test .          # run 180 tests
scala-cli run . --main-class effect.examples.composedExamples
```

## Example

```scala
import effect.effects.*

def pipeline(using State[Int], Writer[String], Raise[LimitExceeded]): Int =
  for i <- 1 to 5 do
    State.modify[Int](_ + 1)
    Writer.tell(s"step ${State.get[Int]}")
    if State.get[Int] > 3 then Raise.raise(LimitExceeded("limit exceeded"))
  State.get[Int]

// Run with nested handlers — nesting order determines semantics
val (log, (state, result)) =
  Writer.handler[String, (state: Int, result: Either[LimitExceeded, Int])]:
    State.handler(0):
      Raise.handler(pipeline)

// state = 4 (persisted — State handler is outside Raise)
// result = Left(LimitExceeded("limit exceeded"))
// log = List("step 1", "step 2", "step 3", "step 4")
```

## Effects

| Effect | Haskell Effective equivalent | Operations | Handler returns |
|---|---|---|---|
| `State[S]` | `Get s` + `Put s` | `get`, `set`, `modify`, `gets` | `(state: S, result: A)` |
| `Reader[R]` | `Ask r` + `Local r` | `ask`, `asks`, `local` | `A` |
| `Writer[W]` | `Tell w` + `Censor w` | `tell`, `censor` | `(output: List[W], result: A)` |
| `Raise[E <: Exception]` | `Throw e` + `Catch e` | `raise`, `catchError`, `retry` | `Either[E, A]` |
| `Fail` | `Throw` (no value) | `fail` | `Option[A]` |
| `Nondet` | `Empty` + `Choose` | `empty`, `choose`, `alt`, `oneOf` | `List[A]` |
| `Amb` | `Alternative` + `Cut` | `choose`, `empty`, `guard`, `cut` | `List[A]` |
| `Console` | `GetLine` + `PutStrLn` | `readLine`, `printLine` | `A` / `(output: List[String], result: A)` |
| `Emit[A]` | `Yield a b` + `MapYield` | `emit`, `mapEmit`, `fold` | `(values: List[A], result: B)` |
| `Timeout` | — | `checkTimeout`, `remaining`, `isExpired` | `Option[A]` |
| `RefStore` | `HStore` (New + Get + Put) | `make` + extension methods on `Ref[A]` | `A` |
| `Async` | `JPar` + `Par` | `fork`, `par`, `race` | `A` |

Error types must extend `Exception`. Domain exception case classes are provided
in `errors.scala`: `EffectError`, `ArithmeticError`, `ValidationError`,
`ParseError`, `LimitExceeded`. Case classes give structural equality for test
assertions.

## Architecture

### Capability traits

Each effect is a trait extending `caps.SharedCapability`:

```scala
trait State[S] extends SharedCapability:
  def get: S
  def set(s: S): Unit
```

The capture checker tracks these in types and prevents escape.

### Primitive operations

Companion objects expose `inline` operations via anonymous `using` clauses:

```scala
object State:
  inline def get[S](using State[S]): S = summon[State[S]].get
  inline def modify[S](f: S => S)(using State[S]): Unit =
    val s = summon[State[S]]
    s.set(f(s.get))
```

The `inline` eliminates the forwarding overhead at call sites. The `using`
parameters are anonymous — no need to name them when they're only passed
through.

### Handlers

Handlers provide a capability implementation, run the program, and return
named tuples:

```scala
def handler[S, A](initial: S)(program: State[S] ?=> A): (state: S, result: A) =
  var current: S = initial
  val cap = new State[S]:
    def get: S = current
    def set(s: S): Unit = current = s
  val result = program(using cap)
  (state = current, result = result)
```

Named tuples allow both positional (`val (s, a) = ...`) and named
(`result.state`, `result.result`) access.

### Extension methods (Ref)

`RefStore` provides extension methods on `Ref[A]` for ergonomic access:

```scala
import effect.effects.RefStore.*

RefStore.handler:
  val counter = RefStore.make(0)
  counter.modify(_ + 1)     // instead of RefStore.modify(counter)(_ + 1)
  counter.get               // instead of RefStore.get(counter)
  counter.set(42)            // instead of RefStore.set(counter, 42)
```

### Handler composition

Composition is nested function application. The nesting order determines
semantics, exactly as in Effective's `|>` (fuse) and `||>` (pipe):

```scala
// State outside Raise = "global state" (state persists through errors)
State.handler(0) { Raise.handler { program } }

// Raise outside State = "local state" (state rolls back on error)
Raise.handler { State.handler(0) { program } }
```

## Effect categories

### Pure state effects

**State**, **Reader**, **Writer**, **Emit**, **RefStore** — use mutable local
variables inside the handler. The anonymous capability only closes over these
vars, which are not tracked by the capture checker.

### Control flow effects

**Raise**, **Fail**, **Timeout** — use token-tagged exceptions for
short-circuiting. Each handler creates a unique `AnyRef` token; the exception
carries the token so nested handlers of the same type catch at the correct level.

We use plain exceptions rather than `boundary`/`break` because `boundary.Label`
extends `caps.Control`, and the capture checker would track the label in the
anonymous class's self-type, making the implementation more complex.

### Nondeterminism

**Nondet**, **Amb** — use **indexed re-execution**. The program runs multiple
times, each following a different "path" of choice indices. A worklist tracks
unexplored paths. Amb extends this with Prolog-style `cut` for search pruning.

Scala lacks multi-shot continuations, so re-execution replaces Haskell Effective's
continuation-cloning approach. Both explore the same search tree.

### Structured concurrency

**Async** — uses JDK 25's `StructuredTaskScope` with virtual threads. Each
`fork` creates a virtual thread delivering results via `CompletableFuture`.
The scope is always joined before close, satisfying the structured concurrency
protocol.

`caps.unsafe.unsafeAssumePure` bridges the capture checking gap with Java's
`Callable` API (safe because structured concurrency guarantees task lifetime
within scope).

```scala
Async.handler:
  val (a, b) = Async.par(
    fetchUser(id),
    fetchOrder(id)
  )
  process(a, b)
```

## Capture checking

All effect traits extend `caps.SharedCapability`. This provides two compile-time
guarantees:

### Capabilities cannot escape their handler scope

```scala
var leaked: State[Int]^ = ...
State.handler(0):
  leaked = summon[State[Int]]  // COMPILE ERROR
  42
```

### SharedCapability self-type restriction

Anonymous classes extending `SharedCapability` have their self-type restricted
to `{cap}`. External references (function parameters) are excluded. Handlers
needing external functions delegate to non-capturing handlers:

```scala
// Direct implementation rejected: `f` not in `{cap}` self-type
// val cap = new Emit[A]:
//   def emit(value: A): Unit = acc = f(acc, value)  // ERROR

// Solution: delegate, then post-process
def fold[A, S, B](initial: S)(f: (S, A) => S)(
    program: Emit[A] ?=> B
): (accumulated: S, result: B) =
  val (values, result) = toList(program)
  (accumulated = values.foldLeft(initial)(f), result = result)
```

## Project structure

```
project.scala                  # scala-cli config (Scala 3.8.1, JDK 25)
.scalafmt.conf                 # scalafmt 3.10.7
src/effects/
  State.scala                  # State[S] — mutable state
  Reader.scala                 # Reader[R] — environment / config
  Writer.scala                 # Writer[W] — log accumulation
  Error.scala                  # Raise[E <: Exception] + Fail — error handling
  errors.scala                 # Domain exception case classes
  Nondet.scala                 # Nondet — nondeterministic choice
  Amb.scala                    # Amb — nondeterminism with cut
  Console.scala                # Console — I/O
  Yield.scala                  # Emit[A] — generators / coroutines
  Timeout.scala                # Timeout — deadline checking
  Ref.scala                    # RefStore + Ref[A] — dynamic mutable cells
  Async.scala                  # Async — structured concurrency (JDK 25)
src/examples/
  Echo.scala                   # Console echo loop
  StateExample.scala           # State: incr, count, fib
  ErrorExample.scala           # Raise: monus, handler ordering
  WriterExample.scala          # Writer: hoppy example with censor
  NondetExample.scala          # Nondet: knapsack, pythagorean triples
  ComposedExample.scala        # Guessing game with 4 effects
test/
  StateTest.scala              # 13 tests
  ReaderTest.scala             # 10 tests
  WriterTest.scala             # 12 tests
  ErrorTest.scala              # 18 tests (14 Raise + 4 Fail)
  NondetTest.scala             # 15 tests
  ConsoleTest.scala            # 9 tests
  EmitTest.scala               # 10 tests
  CompositionTest.scala        # 38 tests (2-way through 6-way composition)
  AsyncTest.scala              # 10 tests
  RefTest.scala                # 9 tests
  TimeoutTest.scala            # 8 tests
  AmbTest.scala                # 9 tests
  NewCompositionTest.scala     # 19 tests (new effects composed)
```

## Comparison with alternatives

| Approach | Effect tracking | Direct style | Control flow | Concurrency | Composition |
|---|---|---|---|---|---|
| **scala-effect** | Capture checking | Yes | Exceptions | JDK 25 virtual threads | Nested handlers |
| Cats Effect / ZIO | Monad types | No (monadic) | Fibers | Green threads | Monad transformers |
| Kyo | Pending types | Partial | Continuations | Fibers | Pending values |
| Plain Scala | None | Yes | Exceptions | Threads / futures | N/A |

## Mapping from Effective

| Effective (Haskell) | scala-effect (Scala 3) |
|---|---|
| `Prog effs a` | `(Cap1, Cap2, ...) ?=> A` |
| `Member eff sig =>` | `using Cap` |
| `Members '[E1, E2] sig =>` | `(using E1, using E2)` |
| `Handler effs oeffs ts a b` | `def handler(...)( ... ?=> A): B` |
| `handle handler program` | `Effect.handler(init)(program)` |
| `h1 \|> h2` (fuse) | `H1.handler { H2.handler { program } }` |
| `Alg sig` (algebraic) | `inline` trait method forwarding |
| `Scp sig` (scoped) | Higher-order methods taking `?=>` blocks |
| `Distr sig` (distributive) | `StructuredTaskScope` + `CompletableFuture` |

## Requirements

- Scala 3.8.1
- JDK 25 (Temurin, downloaded automatically by scala-cli)
- scala-cli >= 1.12
