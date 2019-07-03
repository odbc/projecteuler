package ru.odbc.problems.p101to150.p119

object Solution extends App {

  val result = Stream.from(2).flatMap { digitsCount =>
    val maxPow = Math.floor(digitsCount * Math.log(10) / Math.log(2)).toInt

    (2 to maxPow).foldLeft(Vector.empty[BigInt]) { case (acc, pow) =>
      val from = BigInt { Math.ceil(Math.pow(10, (digitsCount.toDouble - 1.0) / pow)).toInt }
      val to   = BigInt { Math.floor(Math.pow(10, digitsCount.toDouble / pow)).toInt }
      acc ++ (from to to)
        .filter(n => n.pow(pow).toString.map(_.asDigit).sum == n)
        .map(_.pow(pow))
    }.sorted.toStream
  }.drop(29).head

  println(result)
}
