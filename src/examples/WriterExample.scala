package effect.examples

import effect.effects.Writer

/** Writer example — port of Effective's hoppy example with censor.
  *
  * Effective version: hoppy :: () ! '[Tell [String], Censor [String]] hoppy = do tell ["Hello
  * Alfie!"] censor backwards $ do tell ["tortoise"] censor shout $ do tell ["get bigger!"] tell
  * ["Goodbye!"]
  *
  * ghci> handle (censors id |> writer) hoppy (["Hello Alfie!", "esiotrot", "!REGGIB TEG",
  * "Goodbye!"], ())
  */
def hoppy(using Writer[String]): Unit =
  Writer.tell("Hello Alfie!")
  Writer.censor[String, Unit](_.reverse):
    Writer.tell("tortoise")
    Writer.censor[String, Unit](_.toUpperCase):
      Writer.tell("get bigger!")
  Writer.tell("Goodbye!")

/** Simple logging example. */
def processItems(items: List[Int])(using Writer[String]): List[Int] =
  Writer.tell(s"Processing ${items.length} items")
  val results = items.map: item =>
    Writer.tell(s"  Processing item $item")
    item * 2
  Writer.tell("Done processing")
  results

@main def writerExamples(): Unit =
  // Hoppy example
  val (log, _) = Writer.handler(hoppy)
  println("hoppy output:")
  log.foreach(s => println(s"  $s"))
  // Expected:
  //   Hello Alfie!
  //   esiotrot
  //   !REGGIB TEG
  //   Goodbye!

  println()

  // Processing example
  val (processLog, results) = Writer.handler(processItems(List(1, 2, 3)))
  println("process log:")
  processLog.foreach(s => println(s"  $s"))
  println(s"results: $results")
