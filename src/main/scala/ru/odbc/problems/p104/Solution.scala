package ru.odbc.problems.p104

import scala.math.BigDecimal.RoundingMode

object Solution extends App {

  def isPandigital(s: String): Boolean = s.map(_.asDigit).toSet == (1 to 9).toSet

  val fibMod: Stream[BigInt] = BigInt(1) #:: BigInt(2) #:: fibMod.zip(fibMod.tail).map { case (l, r) => (l + r) % BigInt(10).pow(9) }

  val panTail = (BigInt(1) #:: fibMod).zipWithIndex
    .filter { case (f, _) => isPandigital(f.toString) }
    .map(_._2 + 1)

  val sqFive = Math.sqrt(5)
  val phi = BigDecimal((1 + sqFive) / 2)

  val result = panTail.find { n =>
    val d = (phi.pow(n) / sqFive).setScale(0, RoundingMode.HALF_UP)
    isPandigital(d.toString.slice(0, 9))
  }.get

  println(result)
}
