package ru.odbc.problems.p101

import commons.polynomials.Polynomial
import commons.numbers.Ratio

object Solution extends App {

  def lagrange(points: (Ratio, Ratio)*): Polynomial[Ratio] = {
    points.zipWithIndex.foldLeft(Polynomial.toPoly(Ratio(0, 1))) { case (acc, (point, index)) =>
      val l = points.zipWithIndex.foldLeft(Polynomial.toPoly(Ratio(1, 1))) { case (a, (p, i)) =>
        if (i == index) a
        else acc * Polynomial(Vector(points(i) / (points(index) - points(i))))
      }
    }
  }

  val u = Polynomial(Vector(
    Ratio(1, 1),
    Ratio(-1, 1),
    Ratio(1, 1),
    Ratio(-1, 1),
    Ratio(1, 1),
    Ratio(-1, 1),
    Ratio(1, 1),
    Ratio(-1, 1),
    Ratio(1, 1),
    Ratio(-1, 1),
    Ratio(1, 1),
  ))

  val result = (1 to 11).map(n => u(Ratio(n, 1)))

  println(result)
}
