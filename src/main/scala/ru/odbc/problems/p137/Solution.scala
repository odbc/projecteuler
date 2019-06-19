package ru.odbc.problems.p137

import commons.equations.generalizedPell

object Solution extends App {

  val result = generalizedPell(5, -4).tail.map(_._1)
    .filter(n => (n - 1) % 5 == 0)
    .map(n => (n - 1) / 5)

  println(result.drop(14).head)
}
