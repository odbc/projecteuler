package ru.odbc.problems.p101to150.p146

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  def checkNumber(n: BigInt): Boolean =
    List(n + 1, n + 3, n + 7, n + 9, n + 13, n + 27).forall(Primes(_).isPrime) &&
      List(n + 15, n + 19, n + 21, n + 25).forall(!Primes(_).isPrime)

  val limit = BigInt(150000000)

  val ks1  = Stream.from(10, 210).takeWhile(_ < limit).filter(n => checkNumber(BigInt(n).pow(2))).toList
  val ks8  = Stream.from(80, 210).takeWhile(_ < limit).filter(n => checkNumber(BigInt(n).pow(2))).toList
  val ks13  = Stream.from(130, 210).takeWhile(_ < limit).filter(n => checkNumber(BigInt(n).pow(2))).toList
  val ks20  = Stream.from(200, 210).takeWhile(_ < limit).filter(n => checkNumber(BigInt(n).pow(2))).toList

  val result = (ks1 ++ ks8 ++ ks13 ++ ks20).sum

  println(result)
}
