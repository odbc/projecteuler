package ru.odbc.problems.p42

import scala.io.Source

import commons.numbers.isSquare

object Solution extends App {

  val words: List[String] = Source.fromResource("p042_words.txt")
    .getLines.toList.head
    .split(",").map(s => s.substring(1, s.length - 1)).toList

  val result = words.map(w => w.map(_.toInt - 64).sum).count(n => isSquare(8 * n + 1))

  println(result)

}
