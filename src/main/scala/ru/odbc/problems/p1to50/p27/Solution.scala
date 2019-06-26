package ru.odbc.problems.p1to50.p27

import lib.mathematics.numberTheory.numbers.Primes

import scala.collection.BitSet

object Solution extends App {

  val aLimit = 1000
  val bLimit = 1000
  val nLimit = bLimit

  val squares = (0 to nLimit).map(n => (n, n * n)).toMap

  val primeList = BitSet(Primes.sequence.takeWhile(_ <= squares(nLimit) + aLimit * nLimit + bLimit).map(_.toInt).toList: _*)
  val bs = primeList.takeWhile(_ <= bLimit)

  val (_, _, result, _) = (for {
    a <- (-aLimit + 1) until aLimit
    b <- bs
  } yield (a, b, a * b, (0 to b).takeWhile(n => primeList.contains(squares(n) + a * n + b)).size)).maxBy(_._4)

  println(result)
}
