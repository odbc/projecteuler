package ru.odbc.problems.p112

import commons.numbers.naturalsFrom

import scala.annotation.tailrec

object Solution extends App {

  def isBouncy(n: BigInt): Boolean = {
    val pairs = n.toString.sliding(2, 1).toList
    !pairs.forall(s => s(0).asDigit >= s(1).asDigit) && !pairs.forall(s => s(0).asDigit <= s(1).asDigit)
  }

  def proportionNumber(ns: Stream[BigInt], proportion: Double): BigInt = {
    @tailrec
    def go(stream: Stream[BigInt], total: BigInt, bouncy: BigInt): BigInt = {
      val pr = bouncy.toDouble / total.toDouble
      if (pr >= proportion) total
      else if (isBouncy(stream.head)) go(stream.tail, stream.head, bouncy + 1)
      else go(stream.tail, stream.head, bouncy)
    }

    go(ns, 0, 0)
  }

  val result = proportionNumber(naturalsFrom(10), 0.99)

  println(result)
}
