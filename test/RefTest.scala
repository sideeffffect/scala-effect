package effect.tests

import effect.effects.Ref
import effect.effects.RefStore
import effect.effects.RefStore.*

class RefTest extends munit.FunSuite:

  test("make and get"):
    def program(using RefStore): String =
      val r = RefStore.make("hello")
      r.get

    val v = RefStore.handler(program)
    assertEquals(v, "hello")

  test("make, set, get"):
    def program(using RefStore): String =
      val r = RefStore.make("hello")
      r.set("world")
      r.get

    val v = RefStore.handler(program)
    assertEquals(v, "world")

  test("multiple refs are independent"):
    def program(using RefStore): (String, String) =
      val r1 = RefStore.make("a")
      val r2 = RefStore.make("b")
      r1.set("x")
      (r1.get, r2.get)

    val v = RefStore.handler(program)
    assertEquals(v, ("x", "b"))

  test("modify applies function"):
    def program(using RefStore): String =
      val r = RefStore.make("hello")
      r.modify(_.toUpperCase)
      r.get

    val v = RefStore.handler(program)
    assertEquals(v, "HELLO")

  test("getAndSet returns old value"):
    def program(using RefStore): (String, String) =
      val r = RefStore.make("old")
      val prev = r.getAndSet("new")
      val curr = r.get
      (prev, curr)

    val v = RefStore.handler(program)
    assertEquals(v, ("old", "new"))

  test("many refs"):
    def program(using RefStore): List[Integer] =
      val refs = (1 to 10).map(i => RefStore.make(Integer.valueOf(i))).toList
      refs.map(_.get)

    val v = RefStore.handler(program)
    assertEquals(v, (1 to 10).map(Integer.valueOf).toList)

  test("refs can store different types"):
    def program(using RefStore): (String, Integer, java.lang.Boolean) =
      val r1 = RefStore.make("string")
      val r2 = RefStore.make(Integer.valueOf(42))
      val r3 = RefStore.make(java.lang.Boolean.TRUE)
      (r1.get, r2.get, r3.get)

    val v = RefStore.handler(program)
    assertEquals(v, ("string", Integer.valueOf(42), java.lang.Boolean.TRUE))

  test("ref used as counter"):
    def program(using RefStore): Integer =
      val counter = RefStore.make(Integer.valueOf(0))
      for _ <- 1 to 10 do counter.modify(n => Integer.valueOf(n.intValue + 1))
      counter.get

    val v = RefStore.handler(program)
    assertEquals(v, Integer.valueOf(10))

  test("swap two refs"):
    def program(using RefStore): (String, String) =
      val a = RefStore.make("A")
      val b = RefStore.make("B")
      val tmp = a.get
      a.set(b.get)
      b.set(tmp)
      (a.get, b.get)

    val v = RefStore.handler(program)
    assertEquals(v, ("B", "A"))
