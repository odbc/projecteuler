package ru.odbc.problems.p123

import commons.primes.primes

object Solution extends App {

  val limit = BigInt(10).pow(10)
  val result = primes.zipWithIndex
    .filter(_._2 % 2 == 0)
    .dropWhile { case (pn, n) => 2 * (n + 1) * pn <= limit }
    .head._2 + 1

  println(result)
}
