package ru.odbc.problems.p111

import commons.primes.isPrime

object Solution extends App {

  val digits = 10
  val downLimit = BigInt(10).pow(digits - 1)

  val indices = (0 until digits).toVector
  val combs   = (1 until digits).map(n => (n, indices.combinations(n).toVector))

  val result = ('0' to '9').foldLeft(BigInt(0)) { case (sum, digit) =>
    val mask = Vector.fill(digits)(digit)
    sum + combs.foldLeft(BigInt(0)) { case (sumComb, (count, combByCount)) =>
      if (sumComb == 0) {
        val fills = (0L until BigInt(10).pow(count).toLong)
          .map(_.toString)
          .flatMap { s =>
            val fill = if (s.length == count) s else Vector.fill(count - s.length)('0').mkString + s
            combByCount.map { comb =>
              BigInt(comb.zip(fill).foldLeft(mask) { case (v, (i, c)) => v.updated(i, c) }.mkString)
            }
          }
          .filter(p => p > downLimit && isPrime(p) )

        if (fills.isEmpty) sumComb
        else fills.sum
      }
      else sumComb
    }
  }

  println(result)
}
