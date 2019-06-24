package ru.odbc.problems.p101to150.p126

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  val count = 1000

  val result = Stream.from(1).find { limit =>
    val faces = for {
      a <- (limit - 2) / 4 to 1 by -1
      b <- List(a, (limit - 2 * a) / (2 * (a + 1))).min to 1 by -1
      c <- List(b, (limit - 2 * a * b) / (2 * (a + b))).min to 1 by -1
      if 2 * (a * b + b * c + a * c) <= limit
      sum = a + b + c
      sqs = a * a + b * b + c * c
      d = sqs - 2 * sum + limit + 1
      if Naturals.isSquare(d)
      n = 3 - sum + Naturals.sqrt(d)
      if n % 2 == 0
    } yield (a, b, c)

    faces.size == count
  }.getOrElse(0)

  println(result)
}
