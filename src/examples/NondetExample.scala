package effect.examples

import effect.effects.Nondet

/** Nondeterminism example — port of Effective's knapsack.
  *
  * Effective version:
  *   knapsack :: Int -> [Int] -> [Int] ! '[Empty, Choose]
  *   knapsack w vs
  *     | w < 0     = empty
  *     | w == 0    = return []
  *     | otherwise = do
  *         v <- select vs
  *         vs' <- knapsack (w - v) vs
  *         return (v : vs')
  *
  *   ghci> handle list (knapsack 3 [3, 2, 1])
  *   [[3], [2,1], [1,2], [1,1,1]]
  */
def knapsack(weight: Int, values: List[Int])(using Nondet): List[Int] =
  if weight < 0 then Nondet.empty
  else if weight == 0 then Nil
  else
    val v = Nondet.choose(values)
    v :: knapsack(weight - v, values)

/** Pythagorean triples — classic nondeterminism example. */
def pythagorean(limit: Int)(using Nondet): (Int, Int, Int) =
  val a = Nondet.choose((1 to limit).toList)
  val b = Nondet.choose((a to limit).toList)
  val c = Nondet.choose((b to limit).toList)
  if a * a + b * b == c * c then (a, b, c)
  else Nondet.empty

/** Simple coin flip. */
def coin(using Nondet): String =
  Nondet.choose(List("heads", "tails"))

/** Three coin flips, filter for at least 2 heads. */
def luckyFlips(using Nondet): List[String] =
  val c1 = coin
  val c2 = coin
  val c3 = coin
  val flips = List(c1, c2, c3)
  if flips.count(_ == "heads") >= 2 then flips
  else Nondet.empty

@main def nondetExamples(): Unit =
  // Knapsack
  val results = Nondet.handler(knapsack(3, List(3, 2, 1)))
  println(s"knapsack(3, [3,2,1]): $results")
  // Expected: List(List(3), List(2,1), List(1,2), List(1,1,1))

  // Pythagorean triples (small range to keep output manageable)
  val triples = Nondet.handler(pythagorean(15))
  println(s"pythagorean triples up to 15: $triples")

  // Coin flips
  val lucky = Nondet.handler(luckyFlips)
  println(s"lucky flips (>=2 heads): $lucky")
