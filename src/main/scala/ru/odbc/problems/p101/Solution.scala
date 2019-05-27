package ru.odbc.problems.p101

import commons.polynomials.Polynomial
import commons.numbers.Ratio

object Solution extends App {

  def lagrange(points: (Ratio, Ratio)*): Polynomial[Ratio] = {
    points.zipWithIndex.foldLeft(Polynomial.toPoly(Ratio(0, 1))) { case (acc, ((xi, yi), i)) =>
      points.zipWithIndex.foldLeft(Polynomial.toPoly(Ratio(1, 1))) { case (a, ((xj, _), j)) =>
        if (j == i) a
        else a * Polynomial(Vector(xj / (xj - xi), Ratio(1, 1) / (xi - xj)))
      } * yi + acc
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

  val points = (1 to 11).map(n => (Ratio(n, 1), u(Ratio(n, 1))))

  val result = (1 to 10).map(points.take)
    .map(lagrange(_: _*))
    .map { eq =>
      (1 to 11).map(n => eq(Ratio(n, 1)))
        .zip(points.map(_._2))
        .dropWhile { case (l, r) => l == r }
        .head._1
    }.sum

  println(result)
}
