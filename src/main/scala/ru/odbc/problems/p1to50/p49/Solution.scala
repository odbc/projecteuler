package ru.odbc.problems.p1to50.p49

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val result = (1000 to 9999)
    .map(_.toString.permutations.map(_.toInt).filter(n => n > 999 && Primes(n).isPrime).toList)
    .filter(_.size > 2)
    .flatMap(l => l.combinations(3).map(_.sorted).filter(c => c(0) - c(1) == c(1) - c(2)))
    .distinct

  println(result(1).map(_.toString).reduce(_ + _))
}
