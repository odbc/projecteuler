package lib.mathematics.numberTheory.numbers

import Ratio.{ratioNumeric => nums}

/**
  * Numbers can be expressed as (a + b * sqrt(c)) / d
  * for integers a, b, c, d; with d non-zero
  */
case class QuadraticIrrational(a: BigInt, b: BigInt, c: BigInt, d: BigInt) {

  def contFraction: Stream[BigInt] = {
    if (Naturals.isSquare(c)) Ratio(a + b * Naturals.sqrt(c), d).contFraction
    else {
      def coefficients(a: BigInt, x: (Ratio, Ratio)): Stream[(BigInt, (Ratio, Ratio))] = {
        val den  = x._1.num * x._1.num * x._2.den * x._2.den - x._1.den * x._1.den * x._2.num * x._2.num * c
        val revX = (Ratio(x._1.num * x._1.den * x._2.den * x._2.den, den), Ratio(-x._1.den * x._1.den * x._2.num * x._2.den, den))
        val nextA = Math.floor(nums.toDouble(revX._1) + nums.toDouble(revX._2) * Math.sqrt(c.toDouble)).toLong
        (a, x) #:: coefficients(nextA, (revX._1 - Ratio(nextA, 1), revX._2))
      }

      val a0 = Math.floor((a.toDouble + b.toDouble * Math.sqrt(c.toDouble)) / d.toDouble).toLong
      coefficients(a0, (Ratio(a, d) - Ratio(a0, 1), Ratio(b, d))).map(_._1)
    }
  }
}
