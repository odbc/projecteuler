package ru.odbc.problems.p28

import commons.numbers.naturals

object Solution extends App {

  val edge = 1001

  val result = naturals.takeWhile(_ < edge)
    .filter(_ % 2 != 0)
    .flatMap { n =>
      val base = n * n + n + 1
      val diff = n + 1
      List.iterate(base, 4)(_ + diff)
    } sum

  println(result + 1)

}
