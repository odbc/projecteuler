package ru.odbc.problems.p44

import commons.numbers.isSquare
import commons.operations.sqrt

object Solution extends App {

  def isPentagonal(n: Long): Boolean = isSquare(24 * n + 1) && (1 + sqrt(24 * n + 1)) % 6 == 0

  def pentagonals: Stream[Int] = Stream.from(1).map(n => n * (3 * n - 1) / 2)

  val result = pentagonals.zipWithIndex
    .flatMap { case (p, i) => pentagonals.take(i).map((p, _)) }
    .map { case (a, b) => (a, b, a + b, a - b) }
    .filter { case (_, _, s, d) => isPentagonal(s) && isPentagonal(d) }

  println(result.take(1).toList.head._4)

}
