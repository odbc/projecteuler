package ru.odbc.problems.p1to50.p10

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = Primes.sequence.takeWhile(_ < 2000000L).sum

  println(result)

}
