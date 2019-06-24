package ru.odbc.problems.p51to100.p100

import lib.mathematics.numberTheory.diophantine.Pell

object Solution extends App {

  val result = Pell(8, 1).solutions
    .map { case (n, y) => ((n + 1) / 2 + y, y) }
    .dropWhile { case (x, y) => x + y < BigInt(10).pow(12)}
    .head._1

  println(result)

}
