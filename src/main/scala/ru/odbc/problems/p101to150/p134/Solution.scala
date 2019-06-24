package ru.odbc.problems.p101to150.p134

import lib.mathematics.numberTheory.numbers.Primes
import lib.mathematics.numberTheory.numbers.Ratio

object Solution extends App {

  def convNums(fraction: Stream[BigInt]): Stream[BigInt] = {
    lazy val ps: Stream[BigInt] =
      BigInt(1) #:: fraction.head #:: ps.zip(ps.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }
    ps
  }

  val limit = 1000000
  val psCount = Primes.sequence.takeWhile(_ <= limit).size
  val ps = Primes.sequence.slice(2, psCount + 1).sliding(2, 1)
    .map { case p1 #:: p2 #:: _ =>
      val nulls = BigInt(10).pow(p1.toString.length)
      val convs = convNums(Ratio(p2, nulls).contFraction)
      val x0 = BigInt(-1).pow(convs.size) * convs.init.last * (-p1)
      val rem =
        if (x0 >= 0) x0 % p2
        else {
          -((-x0) % p2) + p2
        }

      rem * nulls + p1
    }

  println(ps.sum)
}
