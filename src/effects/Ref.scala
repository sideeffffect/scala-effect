package effect.effects

import caps.SharedCapability

/** Ref effect — dynamic mutable cells. Corresponds to Effective's HStore.
  *
  * In Effective: type New = Alg New_; new :: a -> Prog sig (Ref a) type Get = Alg Get_; get :: Ref
  * a -> Prog sig a type Put = Alg Put_; put :: Ref a -> a -> Prog sig ()
  *
  * Unlike State (which provides a single typed cell), RefStore allows creating arbitrarily many
  * dynamically-typed mutable cells at runtime. This is the effect-system equivalent of Haskell's
  * IORef or Scala's Ref/AtomicReference.
  */
/** A mutable reference cell. The type parameter A ensures type-safe reads/writes. */
final class Ref[A] private[effects] (private[effects] val index: Int)

trait RefStore extends SharedCapability:
  def make[A <: AnyRef](initial: A): Ref[A]
  def get[A <: AnyRef](ref: Ref[A]): A
  def set[A <: AnyRef](ref: Ref[A], value: A): Unit

object RefStore:

  def make[A <: AnyRef](initial: A)(using rs: RefStore): Ref[A] = rs.make(initial)
  def get[A <: AnyRef](ref: Ref[A])(using rs: RefStore): A = rs.get(ref)
  def set[A <: AnyRef](ref: Ref[A], value: A)(using rs: RefStore): Unit = rs.set(ref, value)

  def modify[A <: AnyRef](ref: Ref[A])(f: A => A)(using rs: RefStore): Unit =
    rs.set(ref, f(rs.get(ref)))

  def getAndSet[A <: AnyRef](ref: Ref[A], value: A)(using rs: RefStore): A =
    val old = rs.get(ref)
    rs.set(ref, value)
    old

  /** Handler: run a computation with a fresh mutable store.
    *
    * Corresponds to Effective's: hstore :: Handler [Put, Get, New] '[] '[StateT Mem] a a
    *
    * Internally uses an ArrayBuffer as the store, with Ref[A] indexing into it. Type safety is
    * maintained by the Ref[A] class parameterization.
    */
  def handler[A](program: RefStore ?=> A): A =
    val store = collection.mutable.ArrayBuffer.empty[AnyRef]
    val cap = new RefStore:
      def make[B <: AnyRef](initial: B): Ref[B] =
        val idx = store.length
        store += initial
        Ref[B](idx)

      def get[B <: AnyRef](ref: Ref[B]): B =
        store(ref.index).asInstanceOf[B]

      def set[B <: AnyRef](ref: Ref[B], value: B): Unit =
        store(ref.index) = value
    program(using cap)
