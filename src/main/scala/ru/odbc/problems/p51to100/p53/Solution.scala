package ru.odbc.problems.p51to100.p53

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val result = for {
    n <- BigInt(2) to BigInt(100)
    r <- BigInt(1) until n
    c = ((r + BigInt(1)) to n).product / Naturals.factorial(n - r)
    if c > BigInt(1000000)
  } yield c

  println(result.size)

}
