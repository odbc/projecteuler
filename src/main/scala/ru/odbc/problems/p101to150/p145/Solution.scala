package ru.odbc.problems.p101to150.p145

object Solution extends App {

  val limit = 100000000

  val result = (1 until limit)
    .count(n => n % 10 != 0 && (n + BigInt(n.toString.reverse)).toString.forall(_.asDigit % 2 == 1))

  println(result)
}
