package ru.odbc.problems.p21

import commons.primes.factors

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[BigInt, BigInt]

  val divisors = (1 until 10000) filter { n =>
    val factorSum = cache.getOrElseUpdate(n, factors(n).init.sum)
    factorSum != n && cache.getOrElseUpdate(factorSum, factors(factorSum).init.sum) == n
  }

  println(divisors.sum)

}
