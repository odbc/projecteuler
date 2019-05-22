package ru.odbc.problems.p94

import commons.numbers.isSquare

object Solution extends App {

  /**
    * Based on https://dxdy.ru/download/file.php?id=197
    *
    * as - is equal sides, b - is other
    * triangle altitude - h = sqrt(4 * a^2 - b^2) / 2
    * triangle square - b * h / 2 -> b * sqrt(4 * a^2 - b^2) / 4
    *
    * if b = a - 1
    * triangle square - (a - 1) * sqrt(4 * a^2 - (a - 1)^2) / 4 -> (a - 1) * sqrt(4 * a^2 - (a - 1)^2) / 4 ->
    * -> (a - 1) * sqrt(3 * a^2 + 2 * a - 1) / 4
    * if 3 * a^2 + 2 * a - 1 = n^2 -> (3 * a + 1)^2 - 3 * n^2 = 4 ->
    * -> if 3 * a + 1 = m -> m^2 - 3 * n^2 = 4
    *
    * if b = a + 1
    * triangle square - (a + 1) * sqrt(4 * a^2 - (a + 1)^2) / 4 -> (a + 1) * sqrt(4 * a^2 - (a + 1)^2) / 4 ->
    * -> (a + 1) * sqrt(3 * a^2 - 2 * a - 1) / 4
    * if 3 * a^2 - 2 * a - 1 = q^2 -> (3 * a - 1)^2 - 3 * q^2 = 4 ->
    * -> if 3 * a - 1 = p -> p^2 - 3 * q^2 = 4
    *
    * Equation x^2 - 3 * y^2 = 4
    */

  val A = 3
  val B = 4

  case class Pair(x: BigInt, y: BigInt) {
    def *(other: Pair) = Pair(this.x * other.x + A * this.y * other.y, this.x * other.y + this.y * other.x)

    def toNumber: Double = x.toDouble + y.toDouble * Math.sqrt(A)
  }

  val eps = Pair(2, 1)

  val leftY  = 0
  val rightY = Math.floor(Math.sqrt(B) * (eps.toNumber - Math.pow(eps.toNumber, -1)) / (2 * Math.sqrt(A))).toInt

  val basis = for {
    y <- leftY to rightY
    c = A * y + B
    if isSquare(c)
    x = Math.sqrt(c).toInt
  } yield Pair(x, y)

  def solutions(basisPair: Pair): Stream[Pair] = basisPair #:: solutions(basisPair * eps)

  val result = basis.flatMap { b =>
    solutions(b).takeWhile(_.x + 2 < 1000000000).map { case Pair(x, y) =>
      if ((x - 1) % 3 == 0) {
        val a = (x - 1) / 3
        val sq4times = (a - 1) * y
        val perimeter = x - 2
        (sq4times, perimeter)
      } else {
        val a = (x + 1) / 3
        val sq4times = (a + 1) * y
        val perimeter = x + 2
        (sq4times, perimeter)
      }
    }.filter(p => p._1 > 0 && p._1 % 4 == 0).toList
  }

  println(result.map(_._2).sum)
}
