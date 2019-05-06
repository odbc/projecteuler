package ru.odbc.problems.p41

import commons.primes.isPrime

object Solution extends App {

  val perms = for {
    n <- 1 to 9
    p <- List.range(1, n + 1).mkString.permutations
    if isPrime(p.toInt)
  } yield p

  println(perms.map(_.toInt).max)

}
