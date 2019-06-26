package ru.odbc.problems.p1to50.p23

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val limit = 28123

  val abundants = (1 to limit).filter(n => Factors(n).all.init.sum > n)

  val abundantsSums = (for {
    x <- abundants
    y <- abundants if x + y <= limit
  } yield x + y).toSet

  val result = (1 to limit).toSet.diff(abundantsSums).sum

  println(result)
}
