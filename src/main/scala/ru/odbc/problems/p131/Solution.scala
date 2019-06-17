package ru.odbc.problems.p131

import commons.primes.primes
import commons.numbers.isSquare
import commons.operations.sqrt

object Solution extends App {

  val limit = 1000000
  val ps = primes.takeWhile(_ < limit)

  val result = ps.reverse.count { p =>
    val d = 12 * p - 3
    isSquare(d) && {
      val sqd = sqrt(d)
      (sqd - 3) % 6 == 0
    }
  }

  println(result)
}
