package ru.odbc.problems.p101to150.p137

import lib.mathematics.numberTheory.diophantine.Pell

object Solution extends App {

  val result = Pell(5, -4).solutions.tail.map(_._1)
    .filter(n => (n - 1) % 5 == 0)
    .map(n => (n - 1) / 5)

  println(result.drop(14).head)
}
