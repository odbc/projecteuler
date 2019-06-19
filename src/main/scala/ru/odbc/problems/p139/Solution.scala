package ru.odbc.problems.p139

import commons.equations.generalizedPell
import commons.operations.gcd

object Solution extends App {

  val limit = BigInt(10).pow(8)
  val result = generalizedPell(2, -1).tail
    .flatMap { case (x, k) => Stream((x, k), (-x, -k)) }
    .map { case (x, k) =>
      val mm = k + x
      val nn = k - 1
      if (x > 0) {
        val g = gcd(mm, nn)
        (mm / g, nn / g)
      } else {
        val g = gcd(-mm, -nn)
        (mm / -g, nn / -g)
      }
    }
    .filter { case (m, n) => (m - n) % 2 == 1 }
    .takeWhile { case (m, n) => m * m + n * n < limit }
    .map { case (m, n) =>
      val (a, b, c) = (m * m - n * n, 2 * m * n, m * m + n * n)
      Stream.from(1).takeWhile(k => k * a + k * b + k * c < limit).size
    }.sum

  println(result)
}
