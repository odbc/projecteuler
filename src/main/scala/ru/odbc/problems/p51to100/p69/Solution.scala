package ru.odbc.problems.p51to100.p69

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  def phi(n: BigInt): BigInt = Factors(n).primes.distinct.foldLeft(n)((acc, p) => acc / p * (p - 1) )

  val result = (2 to 1000000).map(n => (n, n.toDouble / phi(n).toLong)).maxBy(_._2)

  println(result._1)

}
