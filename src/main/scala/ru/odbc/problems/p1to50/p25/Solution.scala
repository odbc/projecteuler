package ru.odbc.problems.p1to50.p25

import lib.mathematics.numberTheory.numbers.Fibonacci

object Solution extends App {

  val (_, result) = Fibonacci(0).sequence.zipWithIndex
    .find { case (n, _) => n.toString.length == 1000 }
    .get

  println(result)
}
