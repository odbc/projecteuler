package ru.odbc.problems.p101to150.p118

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val distPrimes = Primes.sequence.takeWhile(_ < 10000)
    .filter { p =>
      val pStr = p.toString
      !pStr.contains('0') && pStr.length == pStr.distinct.length
    }.toList

  def primeSets(digits: Set[Int], minPrime: BigInt): BigInt = {
    if (digits.isEmpty) 0
    else {
      val permsPrimes =
        if (digits.sum % 3 == 0) 0
        else digits.toList.permutations.map(p => BigInt(p.mkString)).filter(_ > minPrime).count(Primes(_).isPrime)
      val maxPrime = BigInt(10).pow(digits.size / 2)
      val ps = distPrimes
        .filter(p => p > minPrime && p < maxPrime && p.toString.map(_.asDigit).forall(digits.contains))

      permsPrimes + ps.map(p => primeSets(digits diff p.toString.map(_.asDigit).toSet, p)).sum
    }
  }

  val result = primeSets((1 to 9).toSet, 1)

  println(result)
}
