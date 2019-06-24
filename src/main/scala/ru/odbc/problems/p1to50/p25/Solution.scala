package ru.odbc.problems.p1to50.p25

import lib.mathematics.numberTheory.numbers.Fibonacci

object Solution extends App {

  val overThousandDigits = Fibonacci().sequence.zipWithIndex filter { case (n, _) => n > BigInt(10).pow(999) }

  val result = overThousandDigits.head

  println(result._2 + 2)

}
