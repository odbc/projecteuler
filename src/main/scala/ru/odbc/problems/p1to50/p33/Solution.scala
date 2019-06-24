package ru.odbc.problems.p1to50.p33

import lib.mathematics.numberTheory.numbers.Naturals

final case class Fraction(num: BigInt, denom: BigInt)

object Fraction {
  def apply(num: BigInt, denom: BigInt): Fraction =
    if (denom == 0) new Fraction(1, 0)
    else {
      val g = Naturals.gcd(num, denom)
      new Fraction(num / g, denom / g)
    }
}

object Solution extends App {

  val fractions = for {
    num   <- 10L to 99L
    numStr = num.toString
    denom <- (num + 1L) to 99L
    denomStr = denom.toString
    fraction = Fraction(num, denom)
    if (numStr(0) == denomStr(0) && Fraction(numStr(1).asDigit.toLong, denomStr(1).asDigit.toLong) == fraction) ||
      (numStr(0) == denomStr(1) && Fraction(numStr(1).asDigit.toLong, denomStr(0).asDigit.toLong) == fraction) ||
      (numStr(1) == denomStr(0) && Fraction(numStr(0).asDigit.toLong, denomStr(1).asDigit.toLong) == fraction) ||
      (numStr(1) == denomStr(1) && numStr(1) != '0' && Fraction(numStr(0).asDigit.toLong, denomStr(0).asDigit.toLong) == fraction)
  } yield (num, denom)

  val result = fractions.reduce((l, r) => (l._1 * r._1, l._2 * r._2))

  println(Fraction(result._1, result._2).denom)
}
