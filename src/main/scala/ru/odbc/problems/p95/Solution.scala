package ru.odbc.problems.p95

import commons.primes.factors

object Solution extends App {

  def chain(from: Int): Stream[Int] = from #:: chain(factors(from).init.sum.toInt)

  val limit = 100

  val result = (2 to limit).map { n =>

  }

  println(chain(220).take(10).toList)

}
