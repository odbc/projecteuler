package ru.odbc.problems.p22

import scala.io.Source

object Solution extends App {

  val names: List[String] = Source.fromResource("p022_names.txt")
    .getLines.toList.head
    .split(",").map(s => s.substring(1, s.length - 1)).toList.sorted

  val result = names.zipWithIndex map { case (n, i) =>
    (i + 1) * n.map(_.toInt - 64).sum
  }

  println(result.sum)

}
