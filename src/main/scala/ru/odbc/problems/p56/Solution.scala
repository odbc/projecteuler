package ru.odbc.problems.p56

object Solution extends App {

  val result = for {
    a <- 1 to 100
    b <- 1 to 100
  } yield BigInt(a).pow(b).toString.map(_.asDigit).sum

  println(result.max)

}
