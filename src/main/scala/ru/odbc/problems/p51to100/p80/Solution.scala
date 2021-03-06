package ru.odbc.problems.p51to100.p80

object Solution extends App {

  val digits = 100

  val result = (2 to 99)
    .filter { n =>
      val f = Math.floor(Math.sqrt(n)).toInt
      f * f != n
    }
    .flatMap { n =>
      val f = Math.floor(Math.sqrt(n)).toInt
      (1 to digits).foldLeft(f.toString) { case (acc, _) =>
        val next = (0 to 9).takeWhile { d =>
          val estString = acc + d.toString
          val est = BigInt(estString)
          est * est < BigInt(n) * BigInt(10).pow(2 * acc.length)
        }.last
        acc + next.toString
      }.init
    }
    .map(_.asDigit).sum

  println(result)
}
