package ru.odbc.problems.p50

import commons.primes.{primes, isPrime}

object Solution extends App {

  val limit = 1000000

  val ps = primes.takeWhile(_ < limit)

  val maxSize = (1 to limit).takeWhile(i => ps.take(i).sum < limit).size

  val result = (maxSize to 2 by -1)
    .map(s => ps.sliding(s, 1).map(_.sum).filter(n => n < limit && isPrime(n)))
    .find(_.nonEmpty)

  println(result.get.toList)

}
