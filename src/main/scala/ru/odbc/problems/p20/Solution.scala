package ru.odbc.problems.p20

object Solution extends App {

  val result = (1 to 100).map(BigInt(_)).product.toString.map(_.asDigit.toLong).sum

  println(result)

}
