package ru.odbc.problems.p135

import commons.primes.factors

object Solution extends App {

  val limit = 1000000
  val result = Stream.from(1).takeWhile(_ < limit).count { n =>
    factors(n).count { p =>
      val q = n / p
      (p + q) % 4 == 0 && (p + 5 * q) % 4 == 0 && {
        val d = (p + q) / 4
        val x = (p + 5 * q) / 4
        x > 0 && (x - d) > 0 && (x - 2 * d) > 0
      }
    } == 10
  }

  println(result)
}
