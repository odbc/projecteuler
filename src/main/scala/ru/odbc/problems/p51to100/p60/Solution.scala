package ru.odbc.problems.p51to100.p60

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  def primePair(p1: BigInt, p2: BigInt): Boolean =
    Primes((p1.toString + p2.toString).toLong).isPrime && Primes((p2.toString + p1.toString).toLong).isPrime

  val result = for {
    p1 <- Primes.sequence
    sector = Primes.sequence.takeWhile(_ < p1).toList
    p2 <- sector
    if primePair(p1, p2)
    p3 <- sector.takeWhile(_ < p2)
    if primePair(p1, p3) && primePair(p2, p3)
    p4 <- sector.takeWhile(_ < p3)
    if primePair(p1, p4) && primePair(p2, p4) && primePair(p3, p4)
    p5 <- sector.takeWhile(_ < p4)
    if primePair(p1, p5) && primePair(p2, p5) && primePair(p3, p5) && primePair(p4, p5)
  } yield List(p1, p2, p3, p4, p5)

  println(result.head.sum)

}
