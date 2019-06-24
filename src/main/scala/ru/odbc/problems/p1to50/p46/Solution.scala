package ru.odbc.problems.p1to50.p46

import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

object Solution extends App {

  val odds = Stream.from(9)
    .filter(n => n % 2 == 1 && !Primes(n).isPrime)
    .map(n => (n, Primes.sequence.drop(1).takeWhile(_ < n).map(p => (n - p) / 2).exists(Naturals.isSquare)))

  println(odds.find(!_._2).get)
}
