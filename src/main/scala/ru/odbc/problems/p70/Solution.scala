package ru.odbc.problems.p70

import commons.primes.primeFactors

object Solution extends App {

  def isPermutated(l: String, r: String): Boolean =
    if (l.length != r.length) false
    else l.sorted.zip(r.sorted).forall { case (lc, rc) => lc == rc }

  def phi(n: BigInt): BigInt = primeFactors(n).distinct.foldLeft(n)((acc, p) => acc / p * (p - 1) )

  val result = (2 to 10000000).map(n => (n, phi(n))).filter { case (n, ph) => isPermutated(n.toString, ph.toString) }

  println(result.minBy { case (n, ph) => n.toDouble / ph.toLong }._1)
}
