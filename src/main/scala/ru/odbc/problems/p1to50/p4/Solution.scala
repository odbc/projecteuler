package ru.odbc.problems.p1to50.p4

object Solution extends App {

  val xs = 100 to 999
  val result = (for {
    x <- xs
    y <- xs
    prod    = x * y
    prodStr = prod.toString
    if prodStr == prodStr.reverse
  } yield prod).max

  println(result)
}
