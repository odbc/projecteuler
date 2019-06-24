package ru.odbc.problems.p1to50.p1

object Solution extends App {

  val limit = 1000

  val sum = (1 until limit).filter(p => p % 3 == 0 || p % 5 == 0).sum

  println(sum)
}
