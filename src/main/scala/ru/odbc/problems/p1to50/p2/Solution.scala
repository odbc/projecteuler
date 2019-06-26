package ru.odbc.problems.p1to50.p2

import lib.mathematics.numberTheory.numbers.Fibonacci

object Solution extends App {

  val limit = 4000000L
  val result = Fibonacci(1, 2).sequence.takeWhile(_ < limit).filter(_ % 2 == 0).sum

  println(result)
}
