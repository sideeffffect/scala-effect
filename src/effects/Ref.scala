package effect.effects

import caps.SharedCapability

/** Ref effect — dynamic mutable cells. Corresponds to Effective's HStore.
  *
  * Unlike State (which provides a single typed cell), RefStore allows creating arbitrarily many
  * dynamically-typed mutable cells at runtime. This is the effect-system equivalent of Haskell's
  * IORef or Scala's Ref/AtomicReference.
  */

/** A mutable reference cell. The type parameter A ensures type-safe reads/writes. */
final class Ref[A] private[effects] (private[effects] val index: Int)

trait RefStore extends SharedCapability:
  def make[A](initial: A): Ref[A]
  def get[A](ref: Ref[A]): A
  def set[A](ref: Ref[A], value: A): Unit

object RefStore:

  inline def make[A](initial: A)(using rs: RefStore): Ref[A] = rs.make(initial)

  /** Extension methods on Ref[A] for ergonomic access: ref.get, ref.set(v), ref.modify(f). */
  extension [A](ref: Ref[A])(using rs: RefStore)
    inline def get: A = rs.get(ref)
    inline def set(value: A): Unit = rs.set(ref, value)
    def modify(f: A => A): Unit = rs.set(ref, f(rs.get(ref)))
    def getAndSet(value: A): A =
      val old = rs.get(ref)
      rs.set(ref, value)
      old

  def handler[A](program: RefStore ?=> A): A =
    val store = collection.mutable.ArrayBuffer.empty[Any]
    val cap = new RefStore:
      def make[B](initial: B): Ref[B] =
        val idx = store.length
        store += initial
        Ref[B](idx)

      def get[B](ref: Ref[B]): B =
        store(ref.index).asInstanceOf[B]

      def set[B](ref: Ref[B], value: B): Unit =
        store(ref.index) = value
    program(using cap)
