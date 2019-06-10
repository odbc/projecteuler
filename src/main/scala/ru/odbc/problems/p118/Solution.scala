package ru.odbc.problems.p118

import commons.primes.primes
import commons.primes.isPrime

object Solution extends App {

  val distPrimes = primes.takeWhile(_ < 10000)
    .filter { p =>
      val pStr = p.toString
      !pStr.contains('0') && pStr.length == pStr.distinct.length
    }.toList

  def primeSets(digits: Set[Int], minPrime: BigInt): BigInt = {
    if (digits.isEmpty) 0
    else {
      val permsPrimes =
        if (digits.sum % 3 == 0) 0
        else digits.toList.permutations.map(p => BigInt(p.mkString)).filter(_ > minPrime).count(isPrime)
      val maxPrime = BigInt(10).pow(digits.size / 2)
      val ps = distPrimes
        .filter(p => p > minPrime && p < maxPrime && p.toString.map(_.asDigit).forall(digits.contains))

      permsPrimes + ps.map(p => primeSets(digits diff p.toString.map(_.asDigit).toSet, p)).sum
    }
  }

  val result = primeSets((1 to 9).toSet, 1)

  println(result)
}
