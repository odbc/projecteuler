package ru.odbc.problems.p5

import commons.operations.gcd

object Solution extends App {

  val numbers = (1L to 20L).toList

  val result = numbers.tail.foldLeft(numbers.head)((a, b) => a * b / gcd(a, b))

  println(result)
}
