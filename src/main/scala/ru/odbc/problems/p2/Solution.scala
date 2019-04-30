package ru.odbc.problems.p2

import commons.numbers.fibonacci

object Solution extends App {

  val limit = 4000000L

  val sum = fibonacci.takeWhile(_ < limit).filter(_ % 2 == 0).sum

  println(sum)
}
