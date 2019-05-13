package ru.odbc.problems.p63

object Solution extends App {

  val result = Stream.from(1).map(p => Stream.from(1)
    .map(n => BigInt(n).pow(p)).takeWhile(_.toString.length <= p).dropWhile(_.toString.length < p).toList)

  println(result.takeWhile(_.nonEmpty).flatten.size)

}
