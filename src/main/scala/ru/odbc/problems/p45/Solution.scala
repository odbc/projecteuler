package ru.odbc.problems.p45

import commons.numbers.isSquare
import commons.operations.sqrt

object Solution extends App {

  def hexagonals: Stream[Int] = Stream.from(1).map(n => n * (2 * n - 1))

  def isPentagonal(n: Long): Boolean = isSquare(24 * n + 1) && (1 + sqrt(24 * n + 1)) % 6 == 0
  def isTriangle(n: Long): Boolean   = isSquare(8 * n + 1)

  val result = hexagonals.filter(h => isPentagonal(h) && isTriangle(h))

  println(result.take(3).toList)
}
