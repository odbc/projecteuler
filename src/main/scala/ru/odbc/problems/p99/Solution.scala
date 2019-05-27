package ru.odbc.problems.p99

import scala.io.Source

object Solution extends App {

  val baseExps = Source.fromResource("p099_base_exp.txt")
    .getLines.map(_.split(",")).map(ss => (ss(0).toDouble, ss(1).toDouble)).toList

  val result = baseExps.zipWithIndex
    .map { case ((base, exp), index) => (index + 1, exp * Math.log(base)) }
    .maxBy(_._2)._1

  println(result)

}
