package ru.odbc.problems.p1to50.p41

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = (for {
    n <- 1 to 9
    p <- List.range(1, n + 1).mkString.permutations
    if Primes(p.toInt).isPrime
  } yield p).map(_.toInt).max

  println(result)
}
