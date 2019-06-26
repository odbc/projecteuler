package ru.odbc.problems.p51to100.p66

import lib.mathematics.numberTheory.diophantine.Pell

object Solution extends App {

  val result = (2 to 1000)
    .filter { d =>
      val sq = Math.sqrt(d).toInt
      sq * sq != d
    }
    .map { d => (d, Pell(d, 1).solutions.head) }
    .maxBy(_._2._1)._1

  println(result)
}
