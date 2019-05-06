package ru.odbc.problems.p25

import commons.numbers.fibonacci

object Solution extends App {

  val overThousandDigits = fibonacci.zipWithIndex filter { case (n, _) => n > BigInt(10).pow(999) }

  val result = overThousandDigits.head

  println(result._2 + 2)

}
