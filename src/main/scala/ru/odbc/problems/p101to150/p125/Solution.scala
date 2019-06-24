package ru.odbc.problems.p101to150.p125

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val limit = 100000000
  val palindromes = (1 to BigInt(10).pow(limit.toString.length / 2).toInt).flatMap { n =>
    val nStr = n.toString
    Vector(nStr + nStr.init.reverse, nStr + nStr.reverse)
  }.map(_.toLong).filter(_ < limit)

  val result = palindromes.filter { p =>
    val nLimit = Math.floor(Math.pow(Math.pow(3, 0.5) * Math.pow(972 * p * p - 1, 0.5) + 54 * p, 1.0 / 3) / Math.pow(3, 2.0 / 3) + 1.0 / (Math.pow(3, 1.0 / 3) * Math.pow(Math.pow(3, 0.5) * Math.pow(972 * p * p - 1, 0.5) + 54 * p, 1.0 / 3))).toLong
    val ns = (2L to nLimit).map(BigInt(_))
      .filter(n => Naturals.isSquare(4 * n * p - n * n * (n * n - 1) / 3))
      .map(n => (n, Naturals.sqrt(4 * n * p - n * n * (n * n - 1) / 3)))
      .filter { case (n, d) => n * (1 - n) + d > 0 && (n * (1 - n) + d) % (2 * n) == 0 }
    ns.nonEmpty
  }.map(BigInt(_)).sum

  println(result)
}
