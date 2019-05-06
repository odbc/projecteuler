package ru.odbc.problems.p49

import commons.primes.isPrime

object Solution extends App {

  val result = (1000 to 9999)
    .map(_.toString.permutations.map(_.toInt).filter(n => n > 999 && isPrime(n)).toList)
    .filter(_.size > 2)
    .flatMap(l => l.combinations(3).map(_.sorted).filter(c => c(0) - c(1) == c(1) - c(2)))
    .distinct

  println(result(1).map(_.toString).reduce(_ + _))

}
