package ru.odbc.problems.p51to100.p77

import scala.collection.mutable
import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = Stream.from(4).dropWhile { n =>
    val cache = mutable.Map.empty[(Int, Int), Int]

    val nums = Primes.sequence.takeWhile(_ < n).map(_.toInt)

    def go(sum: Int, max: Int): Int =
      if (sum == 0) 1
      else nums.filter(c => c <= sum && c <= max).map(c => cache.getOrElseUpdate((sum - c, c), go(sum - c, c))).sum

    go(n, nums.max) <= 5000
  }

  println(result.head)
}
