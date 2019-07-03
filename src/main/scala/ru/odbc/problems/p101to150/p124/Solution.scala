package ru.odbc.problems.p101to150.p124

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val limit = 100000
  val (result, _) = (1 to limit).map(n => (n, Factors(n).canonical.keys.product)).sortBy(_._2).drop(9999).head

  println(result)
}
