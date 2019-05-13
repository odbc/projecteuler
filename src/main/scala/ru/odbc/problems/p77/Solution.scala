package ru.odbc.problems.p77

import scala.collection.mutable
import commons.primes.primes

object Solution extends App {

  val result = Stream.from(4).dropWhile { n =>
    val cache = mutable.Map.empty[(Int, Int), Int]

    val nums = primes.takeWhile(_ < n).map(_.toInt)

    def go(sum: Int, max: Int): Int =
      if (sum == 0) 1
      else nums.filter(c => c <= sum && c <= max).map(c => cache.getOrElseUpdate((sum - c, c), go(sum - c, c))).sum

    go(n, nums.max) <= 5000
  }

  println(result.head)
}
