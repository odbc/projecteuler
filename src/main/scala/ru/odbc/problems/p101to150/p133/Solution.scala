package ru.odbc.problems.p101to150.p133

import lib.mathematics.numberTheory.arithmetic.Factors
import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val limit = 100000
  val ps = Primes.sequence.drop(4).takeWhile(_ < limit)

  val result = Primes.sequence.take(4).sum + ps.filter { p =>
    Factors(Factors(p - 1).all.find(f => BigInt(10).pow(f.toInt) % p == 1).get).primes.exists(f => f != 2 && f != 5)
  }.sum

  println(result)
}
