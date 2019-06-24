package ru.odbc.problems.p1to50.p7

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = Primes.sequence.take(10001).last

  println(result)
}
