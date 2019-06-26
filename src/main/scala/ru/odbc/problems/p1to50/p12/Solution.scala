package ru.odbc.problems.p1to50.p12

import lib.mathematics.numberTheory.arithmetic.Factors
import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val result = Naturals().sequence
    .map(n => n * (n + 1) / 2)
    .dropWhile(Factors(_).all.size < 500)
    .head

  println(result)
}
