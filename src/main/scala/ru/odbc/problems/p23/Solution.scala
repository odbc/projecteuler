package ru.odbc.problems.p23

import commons.primes.factors

object Solution extends App {

  val limit = 28123

  val abundants = (1 to limit).filter(n => factors(n).init.sum > n)

  val abundantsSums = (for {
    x <- abundants
    y <- abundants if x + y <= limit
  } yield x + y).toSet

  println((1 to limit toSet).diff(abundantsSums).sum)

}
