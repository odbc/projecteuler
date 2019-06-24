package ru.odbc.problems.p51to100.p98

import scala.io.Source

import lib.mathematics.numberTheory.numbers.Naturals

object Solution extends App {

  def isAnagram(left: String, right: String): Boolean =
    left != right && left.length == right.length && left.toList.sorted == right.toList.sorted

  val words: List[String] = Source.fromResource("p098_words.txt")
    .getLines.toList.head
    .split(",").map(s => s.substring(1, s.length - 1)).toList

  val anagrams = for {
    l <- words
    r <- words
    if isAnagram(l, r)
  } yield (l, r)

  val maxLength = anagrams.map(_._1.length).max
  val squares = Stream.from(1).map(n => n * n)
    .takeWhile(_ < BigInt(10).pow(maxLength).toLong).toList
    .groupBy(_.toString.length)

  val result = anagrams
    .flatMap { case (l, r) =>
      squares.getOrElse(l.length, Nil)
        .map(sq => l.zip(sq.toString).groupBy(_._1))
        .filter(_.forall(pair => pair._2.size == 1 || pair._2.map(_._2).forall(_ == pair._2.head._2)))
        .map(_.mapValues(_.head._2))
        .filter { map =>
          val values = map.values.toList
          values.distinct == values
        }
        .map { map =>
          (l.map(map.getOrElse(_, 'A')).mkString.toLong, r.map(map.getOrElse(_, 'A')).mkString.toLong)
        }
        .filter(p => p._1.toString.length == l.length && p._2.toString.length == l.length && Naturals.isSquare(BigInt(p._2)))
    }
    .flatMap(p => List(p._1, p._2)).distinct.max

  println(result)
}
