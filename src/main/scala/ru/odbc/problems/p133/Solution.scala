package ru.odbc.problems.p133

import commons.primes.primes
import commons.primes.factors
import commons.primes.primeFactors

object Solution extends App {

  val limit = 100000
  val ps = primes.drop(4).takeWhile(_ < limit)

  val result = primes.take(4).sum + ps.filter { p =>
    primeFactors(factors(p - 1).find(f => BigInt(10).pow(f.toInt) % p == 1).get).exists(f => f != 2 && f != 5)
  }.sum

  println(result)
}
