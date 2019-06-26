package ru.odbc.problems.p51to100.p79

import scala.io.Source

object Solution extends App {

  val input = Source.fromResource("p079_keylog.txt").getLines.toList.distinct

  /**
    * Solved by hand
    */
  val result = "73162890"

  println(result)
}
