package ru.odbc.problems.p73

import commons.operations.gcd

object Solution extends App {

  val result = for {
    n <- 2 until 6000
    ds = ((2 * n + 1) until List(3 * n, 12001).min).count(gcd(n, _) == 1)
  } yield ds

  println(result.sum)

}
