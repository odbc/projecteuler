package ru.odbc.problems.p66

import commons.equations.pell

object Solution extends App {

  val result = (2 to 1000)
    .filter { d =>
      val sq = Math.sqrt(d).toInt
      sq * sq != d
    }
    .map { d => (d, pell(d).head) }

  println(result.maxBy(_._2._1)._1)

}
