package ru.odbc.problems.p1to50.p31

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[(Int, Int), Int]
  val coins = List(200, 100, 50, 20, 10, 5, 2, 1)

  def go(sum: Int, max: Int): Int =
    if (sum == 0) 1
    else coins.filter(c => c <= sum && c <= max).map(c => cache.getOrElseUpdate((sum - c, c), go(sum - c, c))).sum

  val result = go(200, 200)

  println(result)
}
