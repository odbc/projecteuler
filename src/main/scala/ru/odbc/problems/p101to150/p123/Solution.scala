package ru.odbc.problems.p101to150.p123

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val limit = BigInt(10).pow(10)
  val result = Primes.sequence.zipWithIndex
    .filter(_._2 % 2 == 0)
    .dropWhile { case (pn, n) => 2 * (n + 1) * pn <= limit }
    .head._2 + 1

  println(result)
}
