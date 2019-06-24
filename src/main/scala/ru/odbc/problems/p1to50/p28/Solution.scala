package ru.odbc.problems.p1to50.p28

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val edge = 1001

  val result = Naturals().sequence.takeWhile(_ < edge)
    .filter(_ % 2 != 0)
    .flatMap { n =>
      val base = n * n + n + 1
      val diff = n + 1
      List.iterate(base, 4)(_ + diff)
    } sum

  println(result + 1)

}
