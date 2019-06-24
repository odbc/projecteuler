package ru.odbc.problems.p101to150.p132

import lib.mathematics.numberTheory.numbers.Primes
import lib.mathematics.numberTheory.arithmetic.Factors

object Solution extends App {

  def remSum(p: BigInt, exp: Int, expNum: Int): List[BigInt] = {
    val exps = Stream.from(1).takeWhile { n =>
      val pExp = p.pow(n)
      val mul = BigInt(10).pow(exp) % pExp
      (1 to expNum).foldLeft((BigInt(1), BigInt(1))) { case ((cur, sum), _) =>
        val next = cur * mul % pExp
        (next, (sum + next) % pExp)
      }._2 == 0
    }

    List.fill(exps.size)(p)
  }

  def cycleMin(ss: Stream[BigInt]*): Stream[BigInt] = {
    val streams = ss.filter(_.nonEmpty)
    if (streams.isEmpty) Stream.empty
    else {
      val min = streams.map(_.head).min
      val (nextMin, nextNotMin) = streams.partition(_.head == min)
      min #:: cycleMin((nextMin.head.tail +: nextMin.tail) ++ nextNotMin: _*)
    }
  }

  val base = Factors(1111111111).primes.toStream
  val factors1 = Primes.sequence.flatMap { p =>
    remSum(p, 10, 99)
  }
  val factors2 = Primes.sequence.flatMap { p =>
    remSum(p, 1000, 99)
  }
  val factors3 = Primes.sequence.flatMap { p =>
    remSum(p, 100000, 9999)
  }
  val result = cycleMin(base, factors1, factors2, factors3)

  println(result.take(40).sum)
}
