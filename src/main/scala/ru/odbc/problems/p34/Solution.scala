package ru.odbc.problems.p34

object Solution extends App {

  val factorials = (0 to 9).map(n => if (n == 0) (0, 1) else (n, (1 to n).product)).toMap

  val result = (10 to 3000000).filter(n => n.toString.map(c => factorials(c.asDigit)).sum == n)

  println(result.sum)

}
