package ru.odbc.problems.p1to50.p6

object Solution extends App {

  val list = 1L to 100L

  val sumList = list.sum
  val result = sumList * sumList - list.map(p => p * p).sum

  println(result)
}
