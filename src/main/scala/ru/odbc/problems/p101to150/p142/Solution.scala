package ru.odbc.problems.p101to150.p142

import lib.mathematics.numberTheory.diophantine.SumOfSquares
import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val numsSets = Stream.from(1).flatMap { x =>
    val ys = SumOfSquares(2 * x).solutions
      .map { case (a, b) => (if (a > b) a * a - b * b else b * b - a * a) / 2 }
      .filter(y => y > 0 && y < x)

    for {
      y <- ys
      z <- ys
      if y > z && Naturals.isSquare(y + z) && Naturals.isSquare(y - z)
    } yield (x, y, z)
  }

  val nums  = numsSets.head
  val limit = nums._1 + nums._2 + nums._3

  val result = numsSets.takeWhile(_._1 < limit).map { case (x, y, z) => x + y + z }.min

  println(result)
}
