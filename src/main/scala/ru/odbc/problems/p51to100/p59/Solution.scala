package ru.odbc.problems.p51to100.p59

import scala.io.Source

object Solution extends App {

  def cycle[A](as: A*): Stream[A] = {
    def go(xs: A*): Stream[A] = xs.head #:: (if (xs.tail.isEmpty) go(as: _*) else go(xs.tail: _*))
    go(as: _*)
  }

  val codes = Source.fromResource("p059_cipher.txt").getLines.toList.head
    .split(",").map(_.toInt)

  val attempts = for {
    a <- 'a' to 'z'
    b <- 'a' to 'z'
    c <- 'a' to 'z'
    encrypted = codes.zip(cycle(a, b, c).take(20).toList).map { case (code, key) => code ^ key }
  } yield (a, b, c, encrypted.map(_.toChar).mkString)

  val keys = List('e', 'x', 'p')
  val result = codes.zip(cycle(keys: _*).take(codes.length)).map { case (code, key) => code ^ key }.sum

  println(result)
}
