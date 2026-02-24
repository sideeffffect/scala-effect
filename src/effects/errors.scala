package effect.effects

/** Domain exception types for use with Raise[E <: Exception].
  *
  * Case classes provide structural equality, making test assertions natural: assertEquals(result,
  * Left(ValidationError("bad input")))
  */

/** General-purpose error with a message. */
case class EffectError(msg: String) extends Exception(msg)

/** Arithmetic domain errors (division by zero, underflow, etc.) */
case class ArithmeticError(msg: String) extends Exception(msg)

/** Input validation errors. */
case class ValidationError(msg: String) extends Exception(msg)

/** Parse/format errors. */
case class ParseError(msg: String) extends Exception(msg)

/** Threshold/limit exceeded errors. */
case class LimitExceeded(msg: String) extends Exception(msg)
