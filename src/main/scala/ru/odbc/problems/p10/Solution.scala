package ru.odbc.problems.p10

import commons.primes.primes

object Solution extends App {

  val result = primes.takeWhile(_ < 2000000L).sum

  println(result)

}
