package ru.odbc.problems.p101to150.p120

object Solution extends App {

  val result = (3 to 1000).map { a =>
    val x = if (a % 2 == 0) a - 2 else a - 1
    BigInt(a * x)
  }.sum

  println(result)
}
