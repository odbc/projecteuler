package ru.odbc.problems.p43

object Solution extends App {

  val result = "0123456789".permutations filter { p =>
    p.slice(1, 4).toInt % 2 == 0 &&
      p.slice(2, 5).toInt % 3 == 0 &&
      p.slice(3, 6).toInt % 5 == 0 &&
      p.slice(4, 7).toInt % 7 == 0 &&
      p.slice(5, 8).toInt % 11 == 0 &&
      p.slice(6, 9).toInt % 13 == 0 &&
      p.slice(7, 10).toInt % 17 == 0
  }

  println(result.map(_.toLong).sum)

}
