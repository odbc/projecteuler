package ru.odbc.problems.p101to150.p136

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  val limit = 50000000

  val fourMod0 = 2 + Stream.from(4)
    .takeWhile(_ < limit / 4)
    .count(n => n == 4 || Primes(n).isPrime || (n > 8 && n % 4 == 0 && Primes(n / 4).isPrime))

  val fourMod3 = Stream.from(3, 4).takeWhile(_ < limit).count(Primes(_).isPrime)

  val result = fourMod0 + fourMod3

  println(result)
}
