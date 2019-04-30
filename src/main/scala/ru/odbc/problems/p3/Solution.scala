package ru.odbc.problems.p3

import commons.primes.primeFactors

object Solution extends App {

  val number = 600851475143L

  val numberFactors = primeFactors(number)

  println(numberFactors.max)

}
