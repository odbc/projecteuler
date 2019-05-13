package ru.odbc.problems.p71

import commons.operations.gcd

object Solution extends App {

  val result = for {
    d <- 3L to 1000000L
    nclosest = Math.floor(3.toDouble * d / 7).toInt
    n = ((if (d == 7 && nclosest == 3) 2 else nclosest) to 1 by -1).find(gcd(_, d) == 1).get
  } yield (n, d, (3 * d - 7 * n).toDouble / (7 * d))

  println(result.minBy(_._3)._1)

}
