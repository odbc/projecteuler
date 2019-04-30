package ru.odbc.problems.p9

import commons.operations.{sqrt, gcd}

object Solution extends App {

  final case class Pythagoreans(a: Long, b: Long, c: Long)

  val limit = sqrt(1000L)

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
