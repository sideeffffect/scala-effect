package effect.tests

import effect.effects.{Ref, RefStore}

class RefTest extends munit.FunSuite:

  test("make and get"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      RefStore.get(r)
    assertEquals(v, "hello")

  test("make, set, get"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      RefStore.set(r, "world")
      RefStore.get(r)
    assertEquals(v, "world")

  test("multiple refs are independent"):
    val v = RefStore.handler:
      val r1 = RefStore.make("a")
      val r2 = RefStore.make("b")
      RefStore.set(r1, "x")
      (RefStore.get(r1), RefStore.get(r2))
    assertEquals(v, ("x", "b"))

  test("modify applies function"):
    val v = RefStore.handler:
      val r = RefStore.make("hello")
      RefStore.modify(r)(_.toUpperCase)
      RefStore.get(r)
    assertEquals(v, "HELLO")

  test("getAndSet returns old value"):
    val v = RefStore.handler:
      val r = RefStore.make("old")
      val prev = RefStore.getAndSet(r, "new")
      val curr = RefStore.get(r)
      (prev, curr)
    assertEquals(v, ("old", "new"))

  test("many refs"):
    val v = RefStore.handler:
      val refs = (1 to 10).map(i => RefStore.make(Integer.valueOf(i))).toList
      refs.map(RefStore.get)
    assertEquals(v, (1 to 10).map(Integer.valueOf).toList)

  test("refs can store different types"):
    val v = RefStore.handler:
      val r1 = RefStore.make("string")
      val r2 = RefStore.make(Integer.valueOf(42))
      val r3 = RefStore.make(java.lang.Boolean.TRUE)
      (RefStore.get(r1), RefStore.get(r2), RefStore.get(r3))
    assertEquals(v, ("string", Integer.valueOf(42), java.lang.Boolean.TRUE))

  test("ref used as counter"):
    val v = RefStore.handler:
      val counter = RefStore.make(Integer.valueOf(0))
      for _ <- 1 to 10 do RefStore.modify(counter)(n => Integer.valueOf(n.intValue + 1))
      RefStore.get(counter)
    assertEquals(v, Integer.valueOf(10))

  test("swap two refs"):
    val v = RefStore.handler:
      val a = RefStore.make("A")
      val b = RefStore.make("B")
      val tmp = RefStore.get(a)
      RefStore.set(a, RefStore.get(b))
      RefStore.set(b, tmp)
      (RefStore.get(a), RefStore.get(b))
    assertEquals(v, ("B", "A"))
