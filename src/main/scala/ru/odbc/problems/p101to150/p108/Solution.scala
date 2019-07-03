package ru.odbc.problems.p101to150.p108

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  /**
    * Equation (x - n)(y - n) = n ** 2
    */

  val limit = 1000

  val result = Stream.from(1).dropWhile { n =>
    Factors(n).canonical.mapValues(2 * _ + 1).values.product / 2 + 1 <= limit
  }.head

  println(result)
}
