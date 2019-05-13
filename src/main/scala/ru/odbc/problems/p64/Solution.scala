package ru.odbc.problems.p64

import commons.operations.gcd

object Solution extends App {

  final case class Coeffs(a: Int, k1: Int, k2: Int, k3: Int)

  def contFraction(x: Int, coeffs: Coeffs): Stream[Coeffs] = {
    val a = Math.floor((coeffs.k1 * Math.sqrt(x) + coeffs.k2) / coeffs.k3).toInt
    val k1 = coeffs.k1 * coeffs.k3
    val k2 = coeffs.k3 * coeffs.k3 * a - coeffs.k2 * coeffs.k3
    val k3 = coeffs.k1 * coeffs.k1 * x - coeffs.k2 * coeffs.k2 + 2 * coeffs.k2 * coeffs.k3 * a - coeffs.k3 * coeffs.k3 * a * a
    val g = gcd(gcd(k1, k2), k3).toInt
    coeffs #:: contFraction(x, Coeffs(a, k1 / g, k2 / g, k3 / g))
  }

  val result = (2 to 10000)
    .filter { d =>
      val sq = Math.sqrt(d).toInt
      sq * sq != d
    }
    .map { d =>
      val a0 = Math.floor(Math.sqrt(d)).toInt
      val fraction = contFraction(d, Coeffs(a0, 1, a0, d - a0 * a0))

      /** Floyd's cycle-finding algorithm
        * https://en.wikipedia.org/wiki/Cycle_detection
        */
      val ν = fraction.zipWithIndex
        .zip(fraction.zipWithIndex.filter(_._2 % 2 == 0)).tail
        .find { case (t, h) => t._1 == h._1 }
        .get._1._2

      val μ = fraction.zipWithIndex
        .zip(fraction.zipWithIndex.drop(2 * ν))
        .find { case (t, h) => t._1 == h._1 }
        .get._1._2

      val startOnμ = fraction.drop(μ)
      val cycle = startOnμ.head :: startOnμ.tail.takeWhile(_ != startOnμ.head).toList

      (d, cycle.size)
    }

  println(result.count(_._2 % 2 == 1))

}
