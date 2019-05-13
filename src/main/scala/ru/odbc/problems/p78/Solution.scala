package ru.odbc.problems.p78

import scala.collection.mutable

object Solution extends App {

  val pentaCache = mutable.Map.empty[BigInt, BigInt]
  def pentagonal(n: BigInt): BigInt = pentaCache.getOrElseUpdate(n, n * (3 * n - 1) / 2)

  val cache = mutable.Map.empty[BigInt, BigInt]

  def p(n: BigInt): BigInt = cache.getOrElseUpdate(n, {
    if (n < 0) 0
    else if (n == 0) 1
    else {

      /**
        * Euler’s pentagonal number recurrence for the partition function
        * https://projecteuclid.org/download/pdf_1/euclid.rmjm/1181069871
        * https://en.wikipedia.org/wiki/Partition_function_(number_theory)
        */
      Stream.from(1).takeWhile(k => n - pentagonal(k) >= 0)
        .map { k =>
          val sign = if (k % 2 == 0) -1 else 1
          sign * (p(n - pentagonal(k)) + p (n - pentagonal(k) - k))
        } sum
    }
  })

  println(Stream.from(1).dropWhile(p(_) % 1000000 != 0).head)

}
