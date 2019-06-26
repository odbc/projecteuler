package ru.odbc.problems.p1to50.p5

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val numbers = (BigInt(1) to BigInt(20)).toList
  val result = numbers.tail.foldLeft(numbers.head)((a, b) => a * b / Naturals.gcd(a, b))

  println(result)
}
