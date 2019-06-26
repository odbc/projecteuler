package ru.odbc.problems.p1to50.p29

object Solution extends App {

  val result = (for {
    a <- 2 to 100
    b <- 2 to 100
  } yield BigInt(a).pow(b)).toSet.size

  println(result)
}
