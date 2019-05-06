package ru.odbc.problems.p47

import commons.primes.primeFactors

object Solution extends App {

  val result = Stream.from(1)
    .map {
      n => (n, List.range(n, n + 4).map(primeFactors(_).distinct))
    } filter { case (_, l) => l.forall(_.size == 4) }

  println(result.take(1).head._1)

}
