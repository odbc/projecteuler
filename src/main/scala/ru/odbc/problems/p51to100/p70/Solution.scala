package ru.odbc.problems.p51to100.p70

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  def isPermutated(l: String, r: String): Boolean = l.sorted == r.sorted

  def phi(n: BigInt): BigInt = Factors(n).primes.distinct.foldLeft(n)((acc, p) => acc / p * (p - 1) )

  val (result, _) = (2 to 10000000)
    .map(n => (n, phi(n)))
    .filter { case (n, ph) => isPermutated(n.toString, ph.toString) }
    .minBy { case (n, ph) => n.toDouble / ph.toLong }

  println(result)
}
