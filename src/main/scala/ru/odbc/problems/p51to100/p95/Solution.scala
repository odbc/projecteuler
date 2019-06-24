package ru.odbc.problems.p51to100.p95

import scala.collection.mutable

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val cache = mutable.Map[Int, Int]()

  val limit = 1000000

  def chain(from: Int): List[Int] = {
    def go(acc: List[Int]): List[Int] = {
      val next = cache.getOrElseUpdate(acc.head, Factors(acc.head).all.init.sum.toInt)
      if (next >= limit) Nil
      else if (acc.contains(next)) (next :: acc).reverse
      else go(next :: acc)
    }

    go(from :: Nil)
  }

  val result =
    (2 until limit)
      .map(chain)
      .filter(l => l.nonEmpty && l.head == l.last)
      .maxBy(_.size)
      .min

  println(result)
}
