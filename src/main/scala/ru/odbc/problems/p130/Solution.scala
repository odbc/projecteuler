package ru.odbc.problems.p130

import commons.numbers.naturalsFrom
import commons.primes.isPrime

object Solution extends App {

  val count = 25
  val result = naturalsFrom(6).filter(n => n % 2 != 0 && n % 5 != 0 && !isPrime(n))
    .filter { n =>
      def remsFrom(index: BigInt, i: BigInt, sum: BigInt): Stream[(BigInt, BigInt, BigInt)] = {
        val next = i * 10 % n
        (index, i, sum) #:: remsFrom(index + 1, next, (sum + next) % n)
      }
      val A = remsFrom(1, 1, 1).find(_._3 == 0).get._1
      (n - 1) % A == 0
    }

  println(result.take(count).sum)
}
