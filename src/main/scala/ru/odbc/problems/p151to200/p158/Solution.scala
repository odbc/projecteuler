package ru.odbc.problems.p151to200.p158

import scala.collection.mutable

object Solution extends App {

  val binomials = mutable.Map.empty[(Int, Int), Long]

  def binomial(n: Int, k: Int): Long =
    if (n < 0 || k < 0 || k > n) 0
    else if (n == 0) 1
    else binomials.getOrElseUpdate((n, k), binomial(n - 1, k) + binomial(n - 1, k - 1))

  val count = 26
  val pairs = for { x <- 1 to count; y <- x + 1 to count } yield (x, y)

  def mergeMaps(left: Map[Int, Long], right: Map[Int, Long]): Map[Int, Long] =
    right.foldLeft(left) { case (acc, (key, value)) =>
      acc.updated(key, acc.getOrElse(key, 0L) + value)
    }

  val res = pairs.map { case (low, high) =>
    val distance = high - low - 1
    val inside   = (0 to distance).map { n => (n + 2, binomial(distance, n) * BigInt(2).pow(n).toLong) }

    val restLeft    = count - high
    val outsideLeft = (0 to restLeft).map { n => (n, binomial(restLeft, n)) }

    val restRight    = low - 1
    val outsideRight = (0 to restRight).map { n => (n, binomial(restRight, n)) }

    val outside =
      (for {
        ol <- outsideLeft
        or <- outsideRight
      } yield (ol._1 + or._1, ol._2 * or._2)).filterNot(_ == (0, 1))

    val withOutside =
      (for {
        in  <- inside
        out <- outside
      } yield (in._1 + out._1, in._2 * out._2)).groupBy(_._1).mapValues(_.map(_._2).sum)

    mergeMaps(inside.toMap, withOutside)
  }.reduce(mergeMaps).values.max

  println(res)
}
