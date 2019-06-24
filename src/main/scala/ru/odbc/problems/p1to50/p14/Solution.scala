package ru.odbc.problems.p1to50.p14

import lib.mathematics.numberTheory.numbers.Collatz

object Solution extends App {

  val result = (1L to 1000000L).map(n => (n, Collatz(n).sequence.takeWhile(_ > 1).size)).maxBy(_._2)

  println(result._1)
}
