package ru.odbc.problems.p101to150.p140

import lib.mathematics.numberTheory.diophantine.Pell

object Solution extends App {

  val result = Pell(5, 44).solutions.tail.map(_._1)
    .filter(n => (n - 7) % 5 == 0)
    .map(n => (n - 7) / 5)

  println(result.take(30).sum)
}
