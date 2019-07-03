package ru.odbc.problems.p101to150.p130

import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

object Solution extends App {

  val count = 25
  val result = Naturals(6).sequence.filter(n => n % 2 != 0 && n % 5 != 0 && !Primes(n).isPrime)
    .filter { n =>
      def remsFrom(index: BigInt, i: BigInt, sum: BigInt): Stream[(BigInt, BigInt, BigInt)] = {
        val next = i * 10 % n
        (index, i, sum) #:: remsFrom(index + 1, next, (sum + next) % n)
      }
      val A = remsFrom(1, 1, 1).find(_._3 == 0).get._1
      (n - 1) % A == 0
    }.take(count).sum

  println(result)
}
