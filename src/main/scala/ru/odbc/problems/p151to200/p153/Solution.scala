package ru.odbc.problems.p151to200.p153

import lib.mathematics.numberTheory.numbers.Naturals
import lib.mathematics.numberTheory.numbers.Arithmetic

object Solution extends App {

  val limit = Math.pow(10, 8).toLong

  val bEq0 =
    Naturals().sequence.takeWhile(_ <= limit / 3)
      .foldLeft(BigInt(0)) { case (acc, a) => a * Math.floorDiv(limit, a.toLong) + acc } +
      Arithmetic(limit / 3 + 1, 1).sum(limit / 2 - limit / 3) * 2 +
      Arithmetic(limit / 2 + 1, 1).sum(limit / 2)

  val aEqB =
    Naturals().sequence.map(_.toLong)
      .takeWhile(_ <= limit / 4)
      .map { k => k * Math.floorDiv(limit, 2 * k) }
      .foldLeft(0L)(_ + _) * 2 +
      Arithmetic(limit / 4 + 1, 1).sum(limit / 4) * 2

  val aNotEqB =
    (for {
      a <- (1L to Math.sqrt(limit).toLong).toStream
      b <- ((a + 1) to Math.sqrt(limit - a * a).toLong).toStream
      if Naturals.gcd(a, b) == 1
      squares = a * a + b * b
      kAndKsq <-
        Naturals().sequence
          .map { k =>
            val kLong = k.toLong
            (kLong, kLong * squares)
          }
          .takeWhile(_._2 <= limit)
    } yield kAndKsq._1 * (a + b) * Math.floorDiv(limit, kAndKsq._2))
      .foldLeft(0L)(_ + _) * 2

  val res = bEq0 + aEqB + aNotEqB

  println(res)
}
