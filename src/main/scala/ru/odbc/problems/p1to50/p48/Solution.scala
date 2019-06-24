package ru.odbc.problems.p1to50.p48

object Solution extends App {

  val result = (1 to 1000).map(n => BigInt(n).pow(n)).sum.toString

  println(result.slice(result.length - 10, result.length))

}
