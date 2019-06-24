package ru.odbc.problems.p101to150.p121

object Solution extends App {

  val turns = 15

  val redLimit = if (turns % 2 == 0) turns / 2 - 1 else turns / 2
  val combs = (1 to redLimit).flatMap { redCount => (1 to turns).toList.combinations(redCount) }
  val probs = combs.map(_.map(BigInt(_)).product).sum + 1

  val result = (1 to turns + 1).map(BigInt(_)).product / probs

  println(result)
}
