package ru.odbc.problems.p1to50.p3

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val result = Factors(600851475143L).primes.max

  println(result)
}
