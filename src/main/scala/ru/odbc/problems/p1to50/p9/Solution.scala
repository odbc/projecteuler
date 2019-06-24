package ru.odbc.problems.p1to50.p9

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  final case class Pythagoreans(a: BigInt, b: BigInt, c: BigInt)

  val limit = Naturals.sqrt(1000L)

  val ms = limit to 1L by -1L

  val mns = for {
    m <- ms
    n <- ms if m > n
  } yield {
    val sqm = m * m
    val sqn = n * n
    Pythagoreans(sqm - sqn, 2 * m * n, sqm + sqn)
  }

  val result = mns.filter(p => p.a + p.b + p.c == 1000L).map(p => p.a * p.b * p.c)

  println(result)

}
