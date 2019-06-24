package ru.odbc.problems.p1to50.p40

object Solution extends App {

  def champernowne(from: Long): Stream[Int] = from.toString.map(_.asDigit).toStream #::: champernowne(from + 1)

  val sequence = champernowne(0).zipWithIndex

  val d1 = sequence.find(_._2 == 1)
  val d10 = sequence.find(_._2 == 10)
  val d100 = sequence.find(_._2 == 100)
  val d1000 = sequence.find(_._2 == 1000)
  val d10000 = sequence.find(_._2 == 10000)
  val d100000 = sequence.find(_._2 == 100000)
  val d1000000 = sequence.find(_._2 == 1000000)

  println(d1.get._1 * d10.get._1 * d100.get._1 * d1000.get._1 * d10000.get._1 * d100000.get._1 * d1000000.get._1)

}
