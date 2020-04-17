package ru.odbc.problems.p151to200.p157

import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  val res = (1 to 9).map { n =>
    val d = BigInt(10).pow(n).toLong
    val factors = Factors(d).all

    (for {
      xi <- factors.indices
      yi <- factors.indices.filter(_ >= xi)
      x = factors(xi)
      y = factors(yi)
      q = d / x + d / y
      rs <- Factors(q).all.map(f => (f * x, f * y))
    } yield rs).distinct.size
  }.sum

  println(res)
}
