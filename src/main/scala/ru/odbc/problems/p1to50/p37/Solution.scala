package ru.odbc.problems.p1to50.p37

import scala.collection.BitSet

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val primeList = BitSet(Primes.sequence.takeWhile(_ < 1000000).map(_.toInt).toList: _*)

  val result = primeList.filter { p =>
    val pStr = p.toString
    (List.iterate(pStr, pStr.length)(s => s.tail) ++ List.iterate(pStr.init, pStr.length - 1)(s => s.init))
      .distinct
      .forall(pr => primeList.contains(pr.toInt))
  }.drop(4).sum

  println(result)
}
