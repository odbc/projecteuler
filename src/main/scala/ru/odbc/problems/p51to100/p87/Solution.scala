package ru.odbc.problems.p51to100.p87

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val limit = 50000000

  val squares = Primes.sequence.map(p => p * p).takeWhile(_ < limit)
  val cubes   = Primes.sequence.map(p => p * p * p).takeWhile(_ < limit)
  val fours   = Primes.sequence.map(p => p * p * p * p).takeWhile(_ < limit)

  val result = (for {
    s <- squares
    c <- cubes
    f <- fours
  } yield s + c + f).filter(_ < limit).distinct.size

  println(result)

}
