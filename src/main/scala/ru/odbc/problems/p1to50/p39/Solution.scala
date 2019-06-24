package ru.odbc.problems.p1to50.p39

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  final case class Pythagoreans(a: BigInt, b: BigInt, c: BigInt) {
    def perimeter: BigInt = a + b + c
  }

  val ms = BigInt(1) to Naturals.sqrt(1000)

  val mns = for {
    m <- ms
    n <- ms if m > n && (m - n) % 2 == 1 && Naturals.gcd(m, n) == 1
    sqm = m * m
    sqn = n * n
    base = Pythagoreans(sqm - sqn, 2 * m * n, sqm + sqn)
    p <- Stream.from(1).map(n => base.copy(base.a * n, base.b * n, base.c * n)).takeWhile(t => t.perimeter <= 1000)
  } yield p

  println(mns)

  val result = (1 to 1000).map(perimeter => (perimeter, mns.count(p => p.a + p.b + p.c == perimeter))).maxBy(_._2)

  println(result)

}
