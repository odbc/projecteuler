package ru.odbc.problems.p69

import commons.primes.primeFactors

object Solution extends App {

  def phi(n: BigInt): BigInt = primeFactors(n).distinct.foldLeft(n)((acc, p) => acc / p * (p - 1) )

  val result = (2 to 1000000).map(n => (n, n.toDouble / phi(n).toLong)).maxBy(_._2)

  println(result._1)

}
