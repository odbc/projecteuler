package ru.odbc.problems.p14

import commons.numbers.collatz

object Solution extends App {

  val result = (1L to 1000000L).map(n => (n, collatz(n).takeWhile(_ > 1).size)).maxBy(_._2)

  println(result._1)

}
