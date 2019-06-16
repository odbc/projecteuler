package ru.odbc.problems.p129

import commons.numbers.naturalsFrom

object Solution extends App {

  val limit = 1000000
  val result = naturalsFrom(limit).filter(n => n % 2 != 0 && n % 5 != 0)
    .find { n =>
      def remsFrom(index: BigInt, i: BigInt, sum: BigInt): Stream[(BigInt, BigInt, BigInt)] = {
        val next = i * 10 % n
        (index, i, sum) #:: remsFrom(index + 1, next, (sum + next) % n)
      }
      val exceeds = remsFrom(1, 1, 1).find(_._3 == 0).get._1
      exceeds > limit
    }.get

  println(result)
}
