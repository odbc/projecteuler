package ru.odbc.problems.p1to50.p45

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  def hexagonals: Stream[Int] = Stream.from(1).map(n => n * (2 * n - 1))

  def isPentagonal(n: Long): Boolean = Naturals.isSquare(24 * n + 1) && (1 + Naturals.sqrt(24 * n + 1)) % 6 == 0
  def isTriangle(n: Long): Boolean   = Naturals.isSquare(8 * n + 1)

  val result = hexagonals.filter(h => isPentagonal(h) && isTriangle(h))

  println(result.take(3).toList)
}
