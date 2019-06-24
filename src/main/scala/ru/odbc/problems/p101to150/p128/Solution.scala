package ru.odbc.problems.p101to150.p128

import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

object Solution extends App {

  val target = 2000
  val result = (BigInt(1) #:: BigInt(2) #:: Naturals(2).sequence.flatMap { ring =>
    val A =
      if (List(6 * ring - 1, 6 * ring + 1, 12 * ring + 5).count(Primes(_).isPrime) == 3) Stream(3 * ring * (ring - 1) + 2)
      else Stream()
    val Ae =
      if (List(6 * ring - 1, 6 * ring + 5, 12 * ring - 7).count(Primes(_).isPrime) == 3) Stream(3 * ring * (ring + 1) + 1)
      else Stream()
    A ++ Ae
  }).drop(target - 1).head

  println(result)
}
