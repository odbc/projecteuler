package ru.odbc.problems.p127

import commons.numbers.naturalsFrom
import commons.operations.gcd
import commons.primes.canonicalRepresentation
import commons.primes.isPrime
import commons.primes.primes

object Solution extends App {

  val limit = 120000
  val rads = naturalsFrom(BigInt(1)).take(limit - 1)
    .map { c => c -> canonicalRepresentation(c).keys.product }.toMap

  val partLimit = 1000
  val cs = rads.filter { case (c, rad) => !isPrime(c) && c / rad > partLimit }

  val hSum = (for {
    (c, radC) <- cs
    limA = if (c % 2 == 0) c / 2 else c / 2 + 1
    a <- Stream.from(1).takeWhile(_ < limA)
    if gcd(a, c) == 1
    b = c - a
    if gcd(a, b) == 1 && rads.getOrElse(a, BigInt(1)) * rads.getOrElse(b, BigInt(1)) * radC < c
  } yield c).sum

  val ps = primes.takeWhile(_ <= partLimit).toVector
  val combsMaxSize = (1 to ps.size).takeWhile(ps.take(_).product <= partLimit).last
  val psCombs = (2 to combsMaxSize).foldLeft(Map(1 -> ps.map(Vector(_)))) { case (map, size) =>
    map + (size -> map(size - 1).flatMap { v =>
      val pss = ps.dropWhile(_ <= v.last).takeWhile(v.product * _ <= partLimit)
      pss.map(v :+ _)
    })
  }.values.flatten

  val bs = psCombs.foldLeft(Vector.empty[BigInt]) { case (acc, factors) =>
    acc ++ factors.foldLeft(Vector(BigInt(1))) { case (bbs, factor) =>
      bbs ++ bbs.flatMap { a =>
        val maxExp = (Math.log(limit.toDouble / a.toDouble) / Math.log(factor.toDouble)).toInt
        (1 to maxExp).map(a * factor.pow(_))
      }
    }
  }.distinct.sorted

  val lSum = (for {
    b <- bs.tail
    a <- bs.takeWhile(_ < b)
    if gcd(a, b) == 1
    c = a + b
    radC = rads.getOrElse(c, BigInt(1))
    if c < limit && c / radC <= partLimit && gcd(a, c) == 1 &&
       rads.getOrElse(a, BigInt(1)) * rads.getOrElse(b, BigInt(1)) * radC < c
  } yield c).sum

  val result = hSum + lSum

  println(result)
}
