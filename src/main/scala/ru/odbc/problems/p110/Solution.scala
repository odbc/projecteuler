package ru.odbc.problems.p110

import commons.primes.primes

object Solution extends App {

  val limit = 4000000
  val prodLimit = 2 * (limit - 1)
  val prodSize = Math.floor(Math.log(prodLimit) / Math.log(3)).toInt

  val result = (prodSize to 2 by -1).foldLeft((true, BigInt(10).pow(100))) { case ((contFlag, res), size) =>
    if (contFlag) {
      val init = Vector.fill(size)(3)
      val initProd = Vector.fill(size)(3).product

      val minValue = (0 until size).foldLeft((Vector(init), BigInt(10).pow(100))) { case ((acc, min), pos) =>
        val nextAcc =
          if (pos == 0) (5 to 3 * prodLimit / initProd + 2 by 2).map(_ +: Vector.fill(size - 1)(3)).toVector
          else acc.flatMap { v =>
            (5 to 3 * prodLimit / v.updated(pos, 1).product + 2 by 2).filter(_ <= v(pos - 1)).map(v.updated(pos, _))
          }

        val nextMin =
          if (nextAcc.isEmpty) min
          else nextAcc
            .filter(_.product > prodLimit)
            .map {
              _.map(p => (p - 1) / 2).zip(primes.take(size).toVector).map { case (p, b) => b.pow(p) }.product
            }
            .min

        (nextAcc.filter(_.product <= prodLimit), Vector(nextMin, min).min)
      }._2

      if (minValue >= res) (false, res) else (true, minValue)
    } else (contFlag, res)
  }._2

  println(result)
}
