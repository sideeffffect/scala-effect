package effect.tests

import effect.effects.Writer

class WriterTest extends munit.FunSuite:

  test("tell records a single value"):
    val (log, _) = Writer.handler[String, Unit](Writer.tell("hello"))
    assertEquals(log, List("hello"))

  test("multiple tells record in order"):
    val (log, _) = Writer.handler[String, Unit]:
      Writer.tell("a")
      Writer.tell("b")
      Writer.tell("c")
    assertEquals(log, List("a", "b", "c"))

  test("handler returns result alongside log"):
    val (log, v) = Writer.handler[String, Int]:
      Writer.tell("computing")
      42
    assertEquals(log, List("computing"))
    assertEquals(v, 42)

  test("handler_ discards log"):
    val v = Writer.handler_[String, Int]:
      Writer.tell("discarded")
      42
    assertEquals(v, 42)

  test("censor transforms output of a region"):
    val (log, _) = Writer.handler[String, Unit]:
      Writer.tell("before")
      Writer.censor[String, Unit](_.toUpperCase):
        Writer.tell("hello")
        Writer.tell("world")
      Writer.tell("after")
    assertEquals(log, List("before", "HELLO", "WORLD", "after"))

  test("nested censor composes transformations"):
    val (log, _) = Writer.handler[String, Unit]:
      Writer.censor[String, Unit](_.reverse):
        Writer.tell("abc")
        Writer.censor[String, Unit](_.toUpperCase):
          Writer.tell("def")
    assertEquals(log, List("cba", "FED"))

  test("hoppy example from Effective"):
    val (log, _) = Writer.handler[String, Unit]:
      Writer.tell("Hello Alfie!")
      Writer.censor[String, Unit](_.reverse):
        Writer.tell("tortoise")
        Writer.censor[String, Unit](_.toUpperCase):
          Writer.tell("get bigger!")
      Writer.tell("Goodbye!")
    assertEquals(log, List("Hello Alfie!", "esiotrot", "!REGGIB TEG", "Goodbye!"))

  test("empty program produces empty log"):
    val (log, v) = Writer.handler[String, Int](42)
    assertEquals(log, List.empty[String])
    assertEquals(v, 42)

  test("tell with Int type"):
    val (log, _) = Writer.handler[Int, Unit]:
      Writer.tell(1)
      Writer.tell(2)
      Writer.tell(3)
    assertEquals(log, List(1, 2, 3))

  test("handlerWith folds with custom combine"):
    val (total, _) = Writer.handlerWith[Int, Unit](0)(_ + _):
      Writer.tell(10)
      Writer.tell(20)
      Writer.tell(30)
    assertEquals(total, 60)

  test("censor with identity preserves output"):
    val (log, _) = Writer.handler[String, Unit]:
      Writer.censor[String, Unit](identity):
        Writer.tell("preserved")
    assertEquals(log, List("preserved"))

  test("many tells"):
    val (log, _) = Writer.handler[Int, Unit]:
      for i <- 1 to 50 do Writer.tell(i)
    assertEquals(log, (1 to 50).toList)
