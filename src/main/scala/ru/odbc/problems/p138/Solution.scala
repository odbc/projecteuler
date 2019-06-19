package ru.odbc.problems.p138

import commons.equations.generalizedPell

object Solution extends App {

  val result = generalizedPell(5, -1).tail
    .filter { case (x, _) => (x - 2) % 5 == 0 || (x + 2) % 5 == 0 }
    .map(_._2)

  println(result.take(12).sum)
}
