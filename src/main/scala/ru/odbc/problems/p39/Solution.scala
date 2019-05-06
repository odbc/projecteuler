package ru.odbc.problems.p39

import commons.operations.{sqrt, gcd}

object Solution extends App {

  final case class Pythagoreans(a: Long, b: Long, c: Long) {
    def perimeter: Long = a + b + c
  }

  val ms = 1L to sqrt(1000L)

  val mns = for {
    m <- ms
    n <- ms if m > n && (m - n) % 2 == 1 && gcd(m, n) == 1
    sqm = m * m
    sqn = n * n
    base = Pythagoreans(sqm - sqn, 2 * m * n, sqm + sqn)
    p <- Stream.from(1).map(n => base.copy(base.a * n, base.b * n, base.c * n)).takeWhile(t => t.perimeter <= 1000)
  } yield p

  println(mns)

  val result = (1 to 1000).map(perimeter => (perimeter, mns.count(p => p.a + p.b + p.c == perimeter))).maxBy(_._2)

  println(result)

}
