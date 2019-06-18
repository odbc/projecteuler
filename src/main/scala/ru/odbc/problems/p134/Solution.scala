package ru.odbc.problems.p134

import commons.primes.primes
import commons.numbers.Ratio
import commons.numbers.contFraction

object Solution extends App {

  def convNums(fraction: Stream[BigInt]): Stream[BigInt] = {
    lazy val ps: Stream[BigInt] =
      BigInt(1) #:: fraction.head #:: ps.zip(ps.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }
    ps
  }

  val limit = 1000000
  val psCount = primes.takeWhile(_ <= limit).size
  val ps = primes.slice(2, psCount + 1).sliding(2, 1)
    .map { case p1 #:: p2 #:: _ =>
      val nulls = BigInt(10).pow(p1.toString.length)
      val convs = convNums(contFraction(Ratio(p2, nulls)))
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
