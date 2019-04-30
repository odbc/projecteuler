package ru.odbc.problems.p12

import commons.primes.factors
import commons.numbers.naturals

object Solution extends App {

  val result = naturals.map(n => n * (n + 1) / 2).dropWhile(factors(_).size < 500)

  println(result.head)

}
