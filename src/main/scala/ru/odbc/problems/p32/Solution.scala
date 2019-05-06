package ru.odbc.problems.p32

object Solution extends App {

  val digits = List.range(1, 10)
  val perms = digits.permutations flatMap { l =>
    for {
      a <- 1 to 7
      first = l.take(a).mkString.toInt
      b <- 1 to (8 - a)
      second = l.slice(a, a + b).mkString.toInt
      res = l.drop(a + b).mkString.toInt
      if res == first * second
    } yield res
  }

  println(perms.toList.distinct.sum)

}
