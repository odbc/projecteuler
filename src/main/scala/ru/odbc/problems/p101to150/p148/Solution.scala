package ru.odbc.problems.p101to150.p148

import scala.annotation.tailrec

object Solution extends App {

  val p = 7
  val limit = 1000000000

  def inc(n: Vector[Int]): Vector[Int] =
    n.foldRight((true, Vector.empty[Int])) { case (d, (flag, acc)) =>
      val next = if (flag) d + 1 else d
      (next >= p, (if (next >= p) next % p else next) +: acc)
    }._2

  @tailrec
  def loop(step: Int, sum: BigInt, n: Vector[Int]): BigInt =
    if (step == 0) sum
    else loop(step - 1, sum + n.map(_ + 1).product, inc(n))

  val r = Math.floor(Math.log(limit) / Math.log(p)).toInt

  val result = loop(limit - BigInt(p).pow(r).toInt, BigInt(p * (p + 1) / 2).pow(r), 1 +: Vector.fill(r)(0))

  println(result)
}
