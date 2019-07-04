package ru.odbc.problems.p101to150.p143

import lib.mathematics.numberTheory.arithmetic.Factors
import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val limit = 120000
  val pairs = Naturals().sequence.takeWhile(_ <= limit)
    .map { p =>
      val num = 3 * p * p
      val floor = (Math.sqrt(3) * p.toDouble).toInt
      (p, Factors(num).all
        .filter(_ <= floor)
        .map(d => {
          val (s, t) = (num / d, d)
          (s + t, s - t)
        })
        .filter { case (b4, n2) =>
          b4 % 4 == 0 && n2 % 2 == 0 && {
            val n = n2 / 2
            val q = (n - p) / 2
            q > 0 && q < p && (p + q) <= limit
          }
        }
        .map { case (_, n2) => (n2 / 2 - p) / 2 }
      )
    }.toMap

  val result = pairs.flatMap { case (p, qs) =>
    for {
      r <- qs
      q <- qs
      if r < q && p + q + r <= limit && pairs.getOrElse(q, List()).contains(r)
    } yield p + q + r
  }.toSet.sum

  println(result)
}
