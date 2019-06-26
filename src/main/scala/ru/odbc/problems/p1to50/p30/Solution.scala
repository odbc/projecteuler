package ru.odbc.problems.p1to50.p30

object Solution extends App {

  val result = (2 to 500000).filter(n => n.toString.map(c => BigInt(c.asDigit).pow(5)).sum == n).sum

  println(result)
}
