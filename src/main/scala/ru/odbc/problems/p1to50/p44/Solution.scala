package ru.odbc.problems.p1to50.p44

import lib.mathematics.numberTheory.numbers.figurates.Pentagonals

object Solution extends App {

  val (_, result) = Pentagonals.sequence.zipWithIndex
    .flatMap { case (p, i) => Pentagonals.sequence.take(i).map((p, _)) }
    .map { case (a, b) => (a + b, a - b) }
    .filter { case (s, d) => Pentagonals(s).isPentagonal && Pentagonals(d).isPentagonal }
    .head

  println(result)
}
