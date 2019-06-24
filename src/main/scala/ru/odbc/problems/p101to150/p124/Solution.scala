package ru.odbc.problems.p101to150.p124

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val limit = 100000
  val result = (1 to limit).map(n => (n, Factors(n).canonical.keys.product)).sortBy(_._2)

  println(result(9999)._1)
}
