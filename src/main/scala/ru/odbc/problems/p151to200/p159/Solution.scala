package ru.odbc.problems.p151to200.p159

import lib.mathematics.numberTheory.arithmetic.Factors

import scala.collection.mutable

object Solution extends App {

  val limit = 1000000

  val drs   = mutable.Map.empty[Long, Long]
  val mdrss = mutable.Map.empty[Long, Long]

  def getDR(n: Long): Long = drs.getOrElseUpdate(n, n - 9 * Math.floorDiv(n - 1, 9))

  def getMDRS(n: Long): Long =
    mdrss.getOrElseUpdate(n, {
      val allFactors = Factors(n).all.map(_.toLong)
      val allFactorsCount = allFactors.size
      (getDR(n) ::
        allFactors.slice(1, if (allFactorsCount % 2 != 0) allFactorsCount / 2 + 1 else allFactorsCount / 2)
          .zip(allFactors.slice(allFactorsCount / 2, allFactorsCount - 1).reverse)
          .map { case (d, r) => getDR(d) + getMDRS(r) }
      ).max
    })

  val res = (2L until limit).map(getMDRS).sum

  println(res)
}
