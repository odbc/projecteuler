package ru.odbc.problems.p51to100.p86

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val result = Stream.from(10).dropWhile { limit =>
    val ms = (1L to Naturals.sqrt(BigInt(2 * limit)).toLong).toStream

    val paths = for {
      m <- ms
      n <- ms if m > n && (m - n) % 2 == 1 && Naturals.gcd(m, n) == 1
      sqm = m * m
      sqn = n * n
      (_, b, c) = (sqm + sqn, 2 * m * n, sqm - sqn)
      p <- Stream.from(1).map(n => (b * n, c * n)).takeWhile(t => t._1 <= 2 * limit && t._2 <= 2 * limit && (t._1 <= limit || t._2 <= limit))
      (a, bc) <-
      if (p._1 <= limit && p._2 <= limit) List((p._1, p._2), (p._2, p._1))
      else if (p._1 <= limit) List((p._1, p._2))
      else if (p._2 <= limit) List((p._2, p._1))
      else List()
      bcPairsCount = Math.floorDiv(bc, 2)
      count =
        if (a >= bc) bcPairsCount
        else {
          val rest = bcPairsCount - (bc - a - 1)
          if (rest > 0) rest else 0
        }
    } yield count

    paths.sum <= 1000000
  }

  println(result.head)

}
