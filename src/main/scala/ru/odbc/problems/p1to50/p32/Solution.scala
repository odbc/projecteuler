package ru.odbc.problems.p1to50.p32

object Solution extends App {

  val digits = List.range(1, 10)
  val result = digits.permutations.flatMap { l =>
    for {
      a <- 1 to 7
      first = l.take(a).mkString.toInt
      b <- 1 to (8 - a)
      second = l.slice(a, a + b).mkString.toInt
      res = l.drop(a + b).mkString.toInt
      if res == first * second
    } yield res
  }.toList.distinct.sum

  println(result)
}
