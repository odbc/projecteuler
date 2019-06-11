package ru.odbc.problems.p119

object Solution extends App {

  val result = Stream.from(2).flatMap { digitsCount =>
    val maxPow = Math.floor(digitsCount * Math.log(10) / Math.log(2)).toInt

    (2 to maxPow).foldLeft(Stream.empty[BigInt]) { case (acc, pow) =>
      val from = Math.ceil(Math.pow(10, (digitsCount.toDouble - 1.0) / pow)).toInt
      val to = Math.floor(Math.pow(10, digitsCount.toDouble / pow)).toInt
      acc #::: (from to to)
        .map(n => (n, BigInt(n).pow(pow)))
        .filter { case (n, nPow) => nPow.toString.map(_.asDigit).sum == n }
        .map(_._2).toStream
    }.sorted
  }

  println(result.drop(29).head)
}
