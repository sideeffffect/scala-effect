# scala-effect: Algebraic Effects via Scala Capabilities

A direct-style effect system for Scala 3, inspired by the Haskell
[Effective](https://github.com/zenzike/effective) library.

Instead of monads and effect rows, this library uses **Scala 3 capabilities**:
context functions (`?=>`), `caps.SharedCapability`, and experimental capture checking.

All effect traits extend `caps.SharedCapability`, which means the Scala 3 capture
checker tracks them in capture sets and **prevents capabilities from escaping
their handler scope** at compile time.

## Mapping from Effective to Scala

### Core concepts

| Effective (Haskell)           | scala-effect (Scala 3)                       |
|-------------------------------|----------------------------------------------|
| `Prog effs a`                 | `(Cap1, Cap2, ...) ?=> A`                    |
| `Member eff sig =>`           | `using Cap`                                  |
| `Members '[E1, E2] sig =>`    | `(using E1, using E2)`                       |
| `Handler effs oeffs ts a b`   | `def handler(...)( ... ?=> A): B`            |
| `handle handler program`      | `Handler.handler(init)(program)`             |
| `h1 \|> h2` (fuse)            | `H1.handler { H2.handler { program } }`     |
| `h1 \|\|> h2` (pipe)          | same — nesting order = composition order     |
| `Alg sig` (algebraic family)  | Simple trait methods                         |
| `Scp sig` (scoped family)     | Higher-order methods taking `?=>` blocks     |
| `Distr sig` (distributive)    | Not needed — no monad transformer forwarding |

### Effect families

Effective classifies effects into three families that determine how they
interact with monad transformers. Scala capabilities eliminate this
classification entirely:

- **Algebraic** effects (Get, Put, Tell, Ask, Throw, Yield) become simple
  methods on a capability trait. These map trivially.

- **Scoped** effects (Local, Censor, Catch, Once) become higher-order methods
  that take a `?=>` block. In Effective, these require explicit `Forward`
  instances for each monad transformer. In Scala, the scoping is natural.

- **Distributive** effects (JPar) require special handling in Effective.
  In Scala, these would use structured concurrency primitives.

### Handler composition

Effective provides explicit composition operators:

```haskell
-- Parallel composition: run both handlers
except |> state s

-- Sequential composition: output of h1 feeds into h2
tickState ||> state (0 :: Int)
```

In Scala, composition is just nesting:

```scala
// State outside Error = "global state" (state survives errors)
State.handler(0) { Raise.handler { program } }

// Error outside State = "local state" (state rolls back on error)
Raise.handler { State.handler(0) { program } }
```

The nesting order determines semantics, exactly as in Effective.

## Architecture

### Capability trait

Each effect is a trait extending `caps.SharedCapability`. This integrates with
Scala 3's capture checker, which tracks capabilities in types and prevents
them from escaping their handler scope at compile time.

```scala
trait State[S] extends Capability:
  def get: S
  def set(s: S): Unit
```

### Primitive operations

Companion objects provide primitive operations using `using` clauses:

```scala
object State:
  def get[S](using s: State[S]): S = s.get
  def set[S](value: S)(using s: State[S]): Unit = s.set(value)
  def modify[S](f: S => S)(using s: State[S]): Unit = s.set(f(s.get))
```

### Handlers

Handlers are functions that provide a capability implementation:

```scala
def handler[S, A](initial: S)(program: State[S] ?=> A): (S, A) =
  var current: S = initial
  val cap = new State[S]:
    def get: S = current
    def set(s: S): Unit = current = s
  val result = program(using cap)
  (current, result)
```

### Control flow effects

For effects that alter control flow (Error, Nondeterminism), we use
token-tagged exceptions (since `boundary.Label` extends `Control` and would
add its own capture tracking, complicating the anonymous class self-type):

```scala
def handler[E, A](program: Raise[E] ?=> A): Either[E, A] =
  val token = new AnyRef
  val cap = new Raise[E]:
    def raise(error: E): Nothing =
      throw new RaiseException((token, error))
  try Right(program(using cap))
  catch case ex: RaiseException[?] if ... => Left(...)
```

## Effects implemented

| Effect      | Effective equivalent       | Operations                    |
|-------------|---------------------------|-------------------------------|
| `State[S]`  | `Get s` + `Put s`         | `get`, `set`, `modify`, `gets`|
| `Reader[R]` | `Ask r` + `Local r`       | `ask`, `asks`, `local`        |
| `Writer[W]` | `Tell w` + `Censor w`     | `tell`, `censor`              |
| `Raise[E]`  | `Throw e` + `Catch e`     | `raise`, `catchError`, `retry`|
| `Fail`      | `Throw` (no value)        | `fail`                        |
| `Nondet`    | `Empty` + `Choose`        | `empty`, `choose`, `alt`      |
| `Console`   | `GetLine` + `PutStrLn`    | `readLine`, `printLine`       |
| `Emit[A]`   | `Yield a b`               | `emit`, `mapEmit`             |

## Capture checking with `caps.SharedCapability`

All effect traits extend `caps.SharedCapability`. This gives two guarantees:

### 1. Capabilities cannot escape their handler scope

The capture checker prevents capabilities from being stored in variables
or returned from handlers. For example, this is a **compile error**:

```scala
var leaked: State[Int]^ = null.asInstanceOf[State[Int]^]
State.handler(0):
  leaked = summon[State[Int]]  // ERROR: cap not visible from variable leaked
  42
```

Similarly, returning a closure that captures a capability is rejected:

```scala
def leaky(): () => Int =
  State.handler_(0):
    () => State.get[Int]  // ERROR: cap cannot be included in outer capture set
```

### 2. SharedCapability self-type restriction

Anonymous classes extending `SharedCapability` have their self-type restricted
to `{cap}` — their own fresh capability. External references (like function
parameters) are not in the allowed capture set. This means handlers that need
to capture external functions delegate to non-capturing handlers:

```scala
// Direct implementation rejected: `f` not in `{cap}` self-type
// def fold[...](f: ...)(program: ...) =
//   val cap = new Emit[A]:
//     def emit(value: A): Unit = acc = f(acc, value)  // ERROR

// Solution: delegate to toList (which doesn't capture `f`), then fold
def fold[A, S, B](initial: S)(f: (S, A) => S)(program: Emit[A] ?=> B): (S, B) =
  val (values, result) = toList(program)
  (values.foldLeft(initial)(f), result)
```

Handlers that only close over local mutable state (vars) work naturally,
since vars are not external capabilities.

## Nondeterminism: the hard case

Effective handles nondeterminism naturally because Haskell's `Prog` monad
provides **multi-shot continuations**: when `choose` is called, the
continuation can be cloned and run multiple times.

Scala has no multi-shot continuations. Our Nondet handler uses
**indexed re-execution**: the program runs multiple times, each time
following a different "path" of choice indices. A worklist tracks
unexplored paths.

This is semantically equivalent to Effective's approach (both explore
the same search tree) but makes the re-execution explicit.

## Comparison with alternatives

| Approach          | Effect tracking | Direct style | Control flow | Composition      |
|-------------------|-----------------|--------------|--------------|------------------|
| **scala-effect**  | Yes (types)     | Yes          | boundary/break| Nested handlers |
| Cats Effect/ZIO   | Yes (types)     | No (monadic) | Fibers       | Monad transformers|
| Kyo              | Yes (types)     | Partial      | Continuations| Pending values   |
| Plain Scala       | No              | Yes          | Exceptions   | N/A              |

## Running

```bash
# Compile
scala-cli compile .

# Run examples
scala-cli run . --main-class effect.examples.stateExamples
scala-cli run . --main-class effect.examples.errorExamples
scala-cli run . --main-class effect.examples.writerExamples
scala-cli run . --main-class effect.examples.nondetExamples
scala-cli run . --main-class effect.examples.echoTest
scala-cli run . --main-class effect.examples.composedExamples
```
