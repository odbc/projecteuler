package ru.odbc.problems.p101to150.p138

import lib.mathematics.numberTheory.diophantine.Pell

object Solution extends App {

  val result = Pell(5, -1).solutions.tail
    .filter { case (x, _) => (x - 2) % 5 == 0 || (x + 2) % 5 == 0 }
    .map(_._2)
    .take(12).sum

  println(result)
}
