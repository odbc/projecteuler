package ru.odbc.problems.p151to200.p152

import lib.mathematics.numberTheory.numbers.Ratio
import lib.mathematics.numberTheory.numbers.Ratio._
import lib.mathematics.numberTheory.numbers.Primes
import lib.mathematics.numberTheory.arithmetic.Factors

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[(Ratio, Int), BigInt]

  val target = Ratio(1, 4)
  val limit = 80

  val fs = Primes.sequence.takeWhile(_ <= limit / 2).toVector

  val set = (fs.slice(2, fs.length).flatMap { p =>
    val count = limit / p.toInt
    (2 to count).toVector.flatMap { c =>
      (1 to count).combinations(c).toVector
        .map(_.toVector)
        .filter { v =>
          val vect = v.map(n => n * n)
          vect.map(e => vect.filterNot(_ == e).product).sum % (p * p) == 0
        }
    }.flatten.distinct.map(p.toInt * _)
  }.distinct ++ (1 to limit).filter(n => Factors(n).canonical.keys.forall(k => k == 2 || k == 3)))
    .filterNot(List(16, 25, 27, 32, 48, 49, 55, 64, 65, 77).contains)
    .sorted

  val invSquares = set.map(n => (n, Ratio(1, n * n))).toMap
  val restSums = set.zipWithIndex.init.foldRight(Map(set.length - 1 -> invSquares(set.last))) { case ((e, i), sums) =>
    sums + (i -> (sums(i + 1) + invSquares(e)))
  }

  def ways(invSqSum: Ratio, min: Int): BigInt = {
    if (invSqSum === target) 1
    else if (invSqSum > target) 0
    else {
      val nextLimit = target - invSqSum
      (min until set.length).takeWhile(restSums(_) >= nextLimit).map { n =>
        val next = invSqSum + invSquares(set(n))
        cache.getOrElseUpdate((next, n + 1), ways(next, n + 1))
      }.sum
    }
  }

  val result = ways(invSquares(3), min = 2)

  println(result)
}
