package ru.odbc.problems.p140

import commons.equations.generalizedPell

object Solution extends App {

  val result = generalizedPell(5, 44).tail.map(_._1)
    .filter(n => (n - 7) % 5 == 0)
    .map(n => (n - 7) / 5)

  println(result.take(30).sum)
}
