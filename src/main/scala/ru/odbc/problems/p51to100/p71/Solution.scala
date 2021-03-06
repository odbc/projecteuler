package ru.odbc.problems.p51to100.p71

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val (result, _, _) = (for {
    d <- 3L to 1000000L
    nclosest = Math.floor(3.toDouble * d / 7).toInt
    n = ((if (d == 7 && nclosest == 3) 2 else nclosest) to 1 by -1).find(Naturals.gcd(_, d) == 1).get
  } yield (n, d, (3 * d - 7 * n).toDouble / (7 * d))).minBy(_._3)

  println(result)
}
