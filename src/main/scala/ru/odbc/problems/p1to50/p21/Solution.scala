package ru.odbc.problems.p1to50.p21

import lib.mathematics.numberTheory.arithmetic.Factors

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[BigInt, BigInt]

  val divisors = (1 until 10000) filter { n =>
    val factorSum = cache.getOrElseUpdate(n, Factors(n).all.init.sum)
    factorSum != n && cache.getOrElseUpdate(factorSum, Factors(factorSum).all.init.sum) == n
  }

  println(divisors.sum)

}
