package ru.odbc.problems.p51to100.p73

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val result = (for {
    n <- 2 until 6000
    ds = ((2 * n + 1) until List(3 * n, 12001).min).count(Naturals.gcd(n, _) == 1)
  } yield ds).sum

  println(result)
}
