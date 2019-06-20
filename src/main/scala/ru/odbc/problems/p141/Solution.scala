package ru.odbc.problems.p141

import commons.numbers.{naturalsFrom, isSquare}
import commons.primes.{factors, isPrime}

object Solution extends App {

  val limit = BigInt(10).pow(12)
  val ds = (BigInt(2) #:: naturalsFrom(4)
    .takeWhile(_ < Math.pow(limit.toDouble, 0.5).toInt).filterNot(isPrime))
    .flatMap { d =>
      val d2 = d * d
      val d3 = d2 * d
      factors(d2).filter(r => {
        r < d && {
          val nn = d3 / r + r
          nn < limit && isSquare(nn)
        }
      }).map(r => d3 / r + r)
    }.distinct

  println(ds.sum)
}
