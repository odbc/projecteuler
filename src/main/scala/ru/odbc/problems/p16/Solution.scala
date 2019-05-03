package ru.odbc.problems.p16

object Solution extends App {

  val result = BigInt(2).pow(1000).toString().map(_.asDigit.toLong).sum

  println(result)

}
