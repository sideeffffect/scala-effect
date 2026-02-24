package effect.effects

import effect.core.Capability

/** Console effect — corresponds to Effective's GetLine/PutStrLn effects.
  *
  * In Effective: type GetLine = Alg GetLine_ type PutStrLn = Alg PutStrLn_ getLine :: Members
  * '[GetLine] sig => Prog sig String putStrLn :: Members '[PutStrLn] sig => Prog sig ()
  *
  * Both are algebraic effects (simple operations, no scoping). Combined into a single capability in
  * Scala since there's no need for the row-based splitting.
  */
trait Console extends Capability:
  def readLine(): String
  def printLine(s: String): Unit

object Console:

  def readLine()(using c: Console): String = c.readLine()
  def printLine(s: String)(using c: Console): Unit = c.printLine(s)

  /** Handler: real IO-based console.
    *
    * Corresponds to Effective's: teletypeIO :: Handler '[GetLine, PutStrLn] '[Alg IO] '[] a a
    */
  def liveHandler[A](program: Console ?=> A): A =
    val cap = new Console:
      def readLine(): String = scala.io.StdIn.readLine()
      def printLine(s: String): Unit = println(s)
    program(using cap)

  /** Handler: pure console using predetermined input/output.
    *
    * Corresponds to Effective's: getLinePure :: [String] -> Handler '[GetLine] '[] '[StateT
    * [String]] a ([String], a) putStrLnPure :: Handler '[PutStrLn] '[] '[WriterT [String]] a
    * ([String], a)
    *
    * This unifies both into a single pure handler. Returns (remaining input, collected output,
    * result).
    */
  def pureHandler[A](input: List[String])(program: Console ?=> A): (List[String], List[String], A) =
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
    (remainingInput, output.toList, result)

  /** Handler variant returning only the output lines. */
  def testHandler[A](input: List[String])(program: Console ?=> A): (List[String], A) =
    val (_, output, result) = pureHandler(input)(program)
    (output, result)
