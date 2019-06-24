package ru.odbc.problems.p51to100.p75

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  final case class Pythagoreans(a: BigInt, b: BigInt, c: BigInt) {
    def perimeter: BigInt = a + b + c
  }

  val limit = BigInt(1500000)
  val ms = (BigInt(1) to Naturals.sqrt(limit)).toStream

  val mns = for {
    m <- ms
    n <- ms if m > n && (m - n) % 2 == 1 && Naturals.gcd(m, n) == 1
    sqm = m * m
    sqn = n * n
    base = Pythagoreans(sqm - sqn, 2 * m * n, sqm + sqn)
    p <- Stream.from(1).map(n => base.copy(base.a * n, base.b * n, base.c * n)).takeWhile(t => t.perimeter <= limit)
  } yield p

  val result = mns.groupBy(_.perimeter).filter(_._2.size == 1)

  println(result.size)

}
