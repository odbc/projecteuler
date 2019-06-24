package ru.odbc.problems.p51to100.p72

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  def phi(n: BigInt): BigInt = Factors(n).primes.distinct.foldLeft(n)((acc, p) => acc / p * (p - 1) )

  val result = (2 to 1000000).map(n => phi(n)).sum

  println(result)

}
