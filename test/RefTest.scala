package effect.tests

import effect.effects.Ref
import effect.effects.RefStore
import effect.effects.RefStore.*

class RefTest extends munit.FunSuite:

  test("make and get"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      r.get
    assertEquals(v, "hello")

  test("make, set, get"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      r.set("world")
      r.get
    assertEquals(v, "world")

  test("multiple refs are independent"):
    val v = RefStore.handler:
      val r1 = RefStore.make("a")
      val r2 = RefStore.make("b")
      r1.set("x")
      (r1.get, r2.get)
    assertEquals(v, ("x", "b"))

  test("modify applies function"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      r.modify(_.toUpperCase)
      r.get
    assertEquals(v, "HELLO")

  test("getAndSet returns old value"):
    val v = RefStore.handler:
      val r = RefStore.make("old")
      val prev = r.getAndSet("new")
      val curr = r.get
      (prev, curr)
    assertEquals(v, ("old", "new"))

  test("many refs"):
    val v = RefStore.handler:
      val refs = (1 to 10).map(i => RefStore.make(Integer.valueOf(i))).toList
      refs.map(_.get)
    assertEquals(v, (1 to 10).map(Integer.valueOf).toList)

  test("refs can store different types"):
    val v = RefStore.handler:
      val r1 = RefStore.make("string")
      val r2 = RefStore.make(Integer.valueOf(42))
      val r3 = RefStore.make(java.lang.Boolean.TRUE)
      (r1.get, r2.get, r3.get)
    assertEquals(v, ("string", Integer.valueOf(42), java.lang.Boolean.TRUE))

  test("ref used as counter"):
    val v = RefStore.handler:
      val counter = RefStore.make(Integer.valueOf(0))
      for _ <- 1 to 10 do counter.modify(n => Integer.valueOf(n.intValue + 1))
      counter.get
    assertEquals(v, Integer.valueOf(10))

  test("swap two refs"):
    val v = RefStore.handler:
      val a = RefStore.make("A")
      val b = RefStore.make("B")
      val tmp = a.get
      a.set(b.get)
      b.set(tmp)
      (a.get, b.get)
    assertEquals(v, ("B", "A"))
