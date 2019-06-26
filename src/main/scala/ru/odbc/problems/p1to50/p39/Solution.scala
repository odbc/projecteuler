package ru.odbc.problems.p1to50.p39

import lib.mathematics.geometry.Pythagoreans

object Solution extends App {

  val limit = 1000

  val (result, _) = Pythagoreans.sequence
    .takeWhile(_.c <= limit)
    .filter(_.perimeter <= limit)
    .groupBy(_.perimeter)
    .maxBy(_._2.size)

  println(result)
}
