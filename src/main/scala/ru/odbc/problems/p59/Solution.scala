package ru.odbc.problems.p59

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

  attempts.filter(_._4.count(c => !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == ' ')) == 0).foreach(println)

  val keys = List('e', 'x', 'p')
  val result = codes.zip(cycle(keys: _*).take(codes.size)).map { case (code, key) => code ^ key }

  println(result.sum)

}
