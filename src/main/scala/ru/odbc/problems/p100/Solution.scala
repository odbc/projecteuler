package ru.odbc.problems.p100

import commons.equations.pell

object Solution extends App {

  val result = pell(8)
    .map { case (n, y) => ((n + 1) / 2 + y, y) }
    .dropWhile { case (x, y) => x + y < BigInt(10).pow(12)}
    .head._1

  println(result)

}
