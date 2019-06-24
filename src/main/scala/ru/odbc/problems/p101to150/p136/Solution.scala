package ru.odbc.problems.p101to150.p136

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val limit = 50000000
  val result = Stream.from(1).takeWhile(_ < limit).count { n =>
    Factors(n).all.count { p =>
      val q = n / p
      (p + q) % 4 == 0 && (p + 5 * q) % 4 == 0 && {
        val d = (p + q) / 4
        val x = (p + 5 * q) / 4
        x > 0 && (x - d) > 0 && (x - 2 * d) > 0
      }
    } == 1
  }

  println(result)
}
