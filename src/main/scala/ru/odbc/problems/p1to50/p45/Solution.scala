package ru.odbc.problems.p1to50.p45

import lib.mathematics.numberTheory.numbers.figurates.{Hexagonals, Pentagonals}

object Solution extends App {

  val result = Hexagonals.sequence.filter(h => Pentagonals(h).isPentagonal).drop(2).head

  println(result)
}
