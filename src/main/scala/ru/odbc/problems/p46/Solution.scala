package ru.odbc.problems.p46

import commons.primes.{primes, isPrime}
import commons.numbers.isSquare

object Solution extends App {

  val odds = Stream.from(9)
    .filter(n => n % 2 == 1 && !isPrime(n))
    .map(n => (n, primes.drop(1).takeWhile(_ < n).map(p => (n - p) / 2).exists(isSquare)))

  println(odds.find(!_._2).get)

}
