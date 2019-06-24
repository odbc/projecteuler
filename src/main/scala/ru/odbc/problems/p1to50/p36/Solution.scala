package ru.odbc.problems.p1to50.p36

object Solution extends App {

  val result = (1 to 1000000).filter { p =>
    val pStr = p.toString
    val pBin = p.toBinaryString
    pStr == pStr.reverse && pBin == pBin.reverse
  }

  println(result.sum)

}
