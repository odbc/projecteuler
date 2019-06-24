package ru.odbc.problems.p1to50.p47

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val result = Stream.from(1)
    .map {
      n => (n, List.range(n, n + 4).map(Factors(_).primes.distinct))
    } filter { case (_, l) => l.forall(_.size == 4) }

  println(result.take(1).head._1)
}
