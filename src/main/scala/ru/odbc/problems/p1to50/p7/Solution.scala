package ru.odbc.problems.p1to50.p7

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = Primes.sequence.drop(10000).head

  println(result)
}
