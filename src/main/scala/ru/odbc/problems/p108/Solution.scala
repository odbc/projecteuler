package ru.odbc.problems.p108

import commons.primes.canonicalRepresentation

object Solution extends App {

  /**
    * Equation (x - n)(y - n) = n ** 2
    */

  val limit = 1000

  val result = Stream.from(1).dropWhile { n =>
    canonicalRepresentation(BigInt(n)).mapValues(2 * _ + 1).values.product / 2 + 1 <= limit
  }

  println(result.head)
}
