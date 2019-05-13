package ru.odbc.problems.p79

import scala.io.Source

object Solution extends App {

  val input = Source.fromResource("p079_keylog.txt").getLines.toList.distinct

  println(input)

  /**
    * Solved by hand
    */
  println("73162890")

}
