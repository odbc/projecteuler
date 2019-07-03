package ru.odbc.problems.p101to150.p131

import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

object Solution extends App {

  val limit = 1000000
  val ps = Primes.sequence.takeWhile(_ < limit)

  val result = ps.reverse.count { p =>
    val d = 12 * p - 3
    Naturals.isSquare(d) && (Naturals.sqrt(d) - 3) % 6 == 0
  }

  println(result)
}
