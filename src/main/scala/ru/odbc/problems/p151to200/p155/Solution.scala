package ru.odbc.problems.p151to200.p155

import lib.mathematics.numberTheory.numbers.Ratio
import lib.mathematics.numberTheory.numbers.Ratio._

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[Int, Vector[Ratio]]

  val total = 18
  val n = Ratio(120, 1)

  def ways(count: Int): Vector[Ratio] = {
    if (count == 1) Vector(n)
    else
      cache.getOrElseUpdate(
        count,
        (1 to count / 2).toVector.flatMap { left =>
          val right = count - left

          for {
            wl <- cache.getOrElseUpdate(left, ways(left))
            wr <- cache.getOrElseUpdate(right, ways(right))
            r  <- Vector(wl + wr, wl * wr / (wl + wr))
          } yield r
        }.distinct
      )
  }

  val res = (1 to total).flatMap(ways).distinct

  println(res.size)
}
