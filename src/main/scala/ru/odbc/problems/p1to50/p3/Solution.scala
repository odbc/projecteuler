package ru.odbc.problems.p1to50.p3

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val number = 600851475143L

  val numberFactors = Factors(number).primes

  println(numberFactors.max)

}
