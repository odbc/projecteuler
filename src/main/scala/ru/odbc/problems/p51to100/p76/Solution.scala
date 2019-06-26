package ru.odbc.problems.p51to100.p76

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[(Int, Int), Int]

  val nums = (1 until 100).toList

  def go(sum: Int, max: Int): Int =
    if (sum == 0) 1
    else nums.filter(c => c <= sum && c <= max).map(c => cache.getOrElseUpdate((sum - c, c), go(sum - c, c))).sum

  val result = go(100, 99)

  println(result)
}
