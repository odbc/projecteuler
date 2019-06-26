package ru.odbc.problems.p1to50.p26

object Solution extends App {

  def unitFraction(num: Int, denom: Int): Stream[Int] = (num * 10 % denom) #:: unitFraction(num * 10 % denom, denom)

  val (result, _) = (2 until 1000).map { d =>
    val fraction = unitFraction(1, d)

    /** Floyd's cycle-finding algorithm
      * https://en.wikipedia.org/wiki/Cycle_detection
      */
    val ν = fraction.zipWithIndex
      .zip(fraction.zipWithIndex.filter(_._2 % 2 == 0)).tail
      .find { case (t, h) => t._1 == h._1 }
      .get._1._2

    val μ = fraction.zipWithIndex
      .zip(fraction.zipWithIndex.drop(2 * ν))
      .find { case (t, h) => t._1 == h._1 }
      .get._1._2

    val startOnμ = fraction.drop(μ)
    val cycle = startOnμ.head :: startOnμ.tail.takeWhile(_ != startOnμ.head).toList

    (d, cycle.size)
  }.maxBy(_._2)

  println(result)
}
