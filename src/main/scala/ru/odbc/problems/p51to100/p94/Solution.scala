package ru.odbc.problems.p51to100.p94

import lib.mathematics.numberTheory.diophantine.Pell

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

  val result = Pell(3, 4).solutions.takeWhile(_._1 + 2 < 1000000000).map { case (x, y) =>
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
  }.filter(p => p._1 > 0 && p._1 % 4 == 0).map(_._2).sum

  println(result)
}
