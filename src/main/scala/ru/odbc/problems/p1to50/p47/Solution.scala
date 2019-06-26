package ru.odbc.problems.p1to50.p47

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val (result, _) = Stream.from(1)
    .map(n => (n, List.range(n, n + 4).map(Factors(_).primes.distinct)))
    .filter { case (_, l) => l.forall(_.size == 4) }
    .head

  println(result)
}
