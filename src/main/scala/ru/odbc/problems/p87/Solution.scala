package ru.odbc.problems.p87

import commons.primes.primes

object Solution extends App {

  val limit = 50000000

  val squares = primes.map(p => p * p).takeWhile(_ < limit)
  val cubes   = primes.map(p => p * p * p).takeWhile(_ < limit)
  val fours   = primes.map(p => p * p * p * p).takeWhile(_ < limit)

  val result = (for {
    s <- squares
    c <- cubes
    f <- fours
  } yield s + c + f).filter(_ < limit).distinct.size

  println(result)

}
