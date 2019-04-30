package ru.odbc.problems.p7

import commons.primes.primes

object Solution extends App {

  val result = primes.take(10001).last

  println(result)

}
