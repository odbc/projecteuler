package ru.odbc.problems.p101to150.p141

import lib.mathematics.numberTheory.arithmetic.Factors
import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

object Solution extends App {

  val limit = BigInt(10).pow(12)
  val ds = (BigInt(2) #:: Naturals(4).sequence
    .takeWhile(_ < Math.pow(limit.toDouble, 0.5).toInt).filterNot(Primes(_).isPrime))
    .flatMap { d =>
      val d2 = d * d
      val d3 = d2 * d
      Factors(d2).all.filter(r => {
        r < d && {
          val nn = d3 / r + r
          nn < limit && Naturals.isSquare(nn)
        }
      }).map(r => d3 / r + r)
    }.distinct

  println(ds.sum)
}
