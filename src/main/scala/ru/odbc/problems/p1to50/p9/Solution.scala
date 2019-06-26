package ru.odbc.problems.p1to50.p9

import lib.mathematics.geometry.Pythagoreans

object Solution extends App {

  val result = Pythagoreans.sequence.find(_.perimeter == 1000).map(p => p.a * p.b * p.c).get

  println(result)
}
