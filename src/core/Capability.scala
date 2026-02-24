package effect.core

/** Marker trait for all capabilities.
  *
  * In the Effective Haskell library, effects are classified into families:
  *   - Algebraic (Alg): operations that compose trivially (get, put, tell, ask)
  *   - Scoped (Scp): operations that delimit regions (local, catch, censor, once)
  *   - Distributive (Distr): operations needing special distribution (jpar)
  *
  * In Scala with capabilities, we don't need this classification. A capability is simply a trait
  * whose methods represent effectful operations. Context functions (`?=>`) and `boundary`/`break`
  * provide the plumbing.
  */
trait Capability

/** A handler interprets a capability by providing a concrete implementation.
  *
  * In Effective, handlers are composed via:
  *   - `|>` (fuse): parallel composition
  *   - `||>` (pipe): sequential composition
  *
  * In Scala, handler composition is simply nested function application: State.handler(0) {
  * Reader.handler(config) { program } }
  */
object Handler:

  /** Run a program that requires a capability by providing a handler.
    *
    * This is the fundamental operation replacing Effective's `handle` function. The handler
    * provides the capability instance, the program uses it via context functions, and the result is
    * returned.
    */
  inline def run[Cap <: Capability, A](handler: Cap)(program: Cap ?=> A): A =
    program(using handler)
