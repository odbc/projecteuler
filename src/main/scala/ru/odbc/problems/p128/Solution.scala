package ru.odbc.problems.p128

import commons.numbers.naturalsFrom
import commons.primes.isPrime

object Solution extends App {

  val target = 2000
  val result = (BigInt(1) #:: BigInt(2) #:: naturalsFrom(2).flatMap { ring =>
    val A =
      if (List(6 * ring - 1, 6 * ring + 1, 12 * ring + 5).count(isPrime) == 3) Stream(3 * ring * (ring - 1) + 2)
      else Stream()
    val Ae =
      if (List(6 * ring - 1, 6 * ring + 5, 12 * ring - 7).count(isPrime) == 3) Stream(3 * ring * (ring + 1) + 1)
      else Stream()
    A ++ Ae
  }).drop(target - 1).head

  println(result)
}
