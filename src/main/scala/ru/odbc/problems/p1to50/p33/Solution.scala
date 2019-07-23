package ru.odbc.problems.p1to50.p33

import lib.mathematics.numberTheory.numbers.Ratio

object Solution extends App {

  val fractions = for {
    num   <- (BigInt(10) to BigInt(99)).toList
    numStr = num.toString
    denom <- (num + 1) to BigInt(99)
    denomStr = denom.toString
    fraction = Ratio(num, denom)
    if (numStr(0) == denomStr(0) && Ratio(numStr(1).asDigit, denomStr(1).asDigit) === fraction) ||
       (numStr(0) == denomStr(1) && Ratio(numStr(1).asDigit, denomStr(0).asDigit) === fraction) ||
       (numStr(1) == denomStr(0) && Ratio(numStr(0).asDigit, denomStr(1).asDigit) === fraction) ||
       (numStr(1) == denomStr(1) && numStr(1) != '0' && Ratio(numStr(0).asDigit, denomStr(0).asDigit) === fraction)
  } yield fraction

  val result = fractions.product.den

  println(result)
}
