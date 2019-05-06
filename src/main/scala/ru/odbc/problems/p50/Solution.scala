package ru.odbc.problems.p50

import commons.primes.primes

object Solution extends App {

  val ps = primes.takeWhile(_ < 1000000)
    .map(p => {
      println(p)
      val l = primes.takeWhile(_ < p)
      (p, (l.length - 1 to 2 by -1).flatMap(l.sliding(_, 1)).filter(_.sum == p))
    })
    /*.map { case (p, l) =>
      (p, (l.length - 1 to 2 by -1).flatMap(l.sliding(_, 1).toList).filter(_.sum == p))
    }
    .filter { case (_, l) => l.nonEmpty }
    .map { case (p, l) => (p, l.maxBy(_.size).size) }*/

  println(ps.toList)

}
