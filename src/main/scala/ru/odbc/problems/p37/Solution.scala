package ru.odbc.problems.p37

import scala.collection.BitSet

import commons.primes.primes

object Solution extends App {

  val primeList = BitSet(primes.takeWhile(_ < 1000000).map(_.toInt).toList: _*)

  val result = primeList.filter { p =>
    val pStr = p.toString
    (List.iterate(pStr, pStr.length)(s => s.tail) ++ List.iterate(pStr.init, pStr.length - 1)(s => s.init))
      .distinct
      .forall(pr => primeList.contains(pr.toInt))
  }

  println(result.drop(4).sum)

}
