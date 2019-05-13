package ru.odbc.problems.p66

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

  val result = (2 to 1000)
    .filter { d =>
      val sq = Math.sqrt(d).toInt
      sq * sq != d
    }
    .map { d =>
      val a0 = Math.floor(Math.sqrt(d)).toInt
      val fraction = contFraction(d, Coeffs(a0, 1, a0, d - a0 * a0)).map(_.a)

      lazy val ps: Stream[BigInt] = BigInt(1) #:: BigInt(fraction.head) #:: ps.zip(ps.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }
      lazy val qs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: qs.zip(qs.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }

      (d, ps.tail.zip(qs.tail).dropWhile { case (p, q) => p * p - d * q * q != 1 } head)
    }

  println(result.maxBy(_._2._1))

}
