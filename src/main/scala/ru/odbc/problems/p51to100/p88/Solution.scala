package ru.odbc.problems.p51to100.p88

import lib.mathematics.numberTheory.arithmetic.Factors

import scala.collection.mutable

object Solution extends App {

  val factorsCache = mutable.Map[Int, List[Int]]()

  def decomp(n: Int): List[List[Int]] = {
    def go(n: Int, minFactor: Int): List[List[Int]] = {
      if (n == 1) List(List())
      else {
        val fs = factorsCache.getOrElseUpdate(n, Factors(n).all.map(_.toInt)).dropWhile(_ < minFactor)

        fs.flatMap(d => go(n / d, d).map(d :: _))
      }
    }

    go(n, minFactor = 2)
  }

  val limit = 12000
  val ks = mutable.Map((2 to limit).map((_, 0)): _*)

  Stream.from(0)
    .takeWhile { n =>
      decomp(n)
        .map(l => l.size + n - l.sum)
        .foreach { e =>
          ks.get(e).foreach(_ => if (ks(e) == 0) ks(e) = n)
        }

      ks.exists(_._2 == 0)
    }.force

  val result = ks.values.toList.distinct.sum

  println(result)
}
