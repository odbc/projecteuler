package ru.odbc.problems.p124

import commons.primes.canonicalRepresentation

object Solution extends App {

  val limit = 100000
  val result = (1 to limit).map(n => (n, canonicalRepresentation(n).keys.product)).sortBy(_._2)

  println(result(9999)._1)
}
