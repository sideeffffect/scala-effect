package effect.effects

import caps.SharedCapability

/** Console effect — corresponds to Effective's GetLine/PutStrLn effects.
  *
  * Both are algebraic effects (simple operations, no scoping). Combined into a single capability in
  * Scala since there's no need for the row-based splitting.
  */
trait Console extends SharedCapability:
  def readLine(): String
  def printLine(s: String): Unit

object Console:

  inline def readLine()(using Console): String = summon[Console].readLine()
  inline def printLine(s: String)(using Console): Unit = summon[Console].printLine(s)

  def liveHandler[A](program: Console ?=> A): A =
    val cap = new Console:
      def readLine(): String = scala.io.StdIn.readLine()
      def printLine(s: String): Unit = println(s)
    program(using cap)

  /** Handler: pure console using predetermined input/output.
    *
    * Returns a named tuple of (remainingInput, output, result).
    */
  def pureHandler[A](
      input: List[String]
  )(program: Console ?=> A): (remainingInput: List[String], output: List[String], result: A) =
    var remainingInput = input
    val output = collection.mutable.ListBuffer.empty[String]
    val cap = new Console:
      def readLine(): String =
        remainingInput match
          case h :: t =>
            remainingInput = t
            h
          case Nil => ""
      def printLine(s: String): Unit = output += s
    val result = program(using cap)
    (remainingInput = remainingInput, output = output.toList, result = result)

  /** Handler variant returning only the output lines and result. */
  def testHandler[A](input: List[String])(
      program: Console ?=> A
  ): (output: List[String], result: A) =
    val r = pureHandler(input)(program)
    (output = r.output, result = r.result)
