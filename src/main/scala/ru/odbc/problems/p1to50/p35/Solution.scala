package ru.odbc.problems.p1to50.p35

import scala.collection.BitSet

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val primeList = BitSet(Primes.sequence.takeWhile(_ < 1000000).map(_.toInt).toList: _*)

  val result = primeList.count { p =>
    val pStr = p.toString
    List.iterate(pStr, pStr.length)(s => s.tail + s.head).forall(pr => primeList.contains(pr.toInt))
  }

  println(result)
}
