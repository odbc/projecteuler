package ru.odbc.problems.p151to200.p154

import scala.annotation.tailrec

object Solution extends App {

  val limit = 200000
  val multPow = 12

  def factMaxPow(n: Int, k: Int): Int = {
    @tailrec
    def go(div: Int, acc: Int): Int =
      if (div == 0) acc
      else go(div / k, acc + div / k)

    go(n, 0)
  }

  def maxPow(n: Int, k: Int): Int = {
    @tailrec
    def go(div: Int, acc: Int): Int =
      if (div == 0) acc
      else if (div % k != 0) acc
      else go(div / k, acc + 1)

    go(n, 0)
  }

  implicit def bool2long(b: Boolean): Long = if (b) 1 else 0

  case class TwosAndFives(twos: Int, fives: Int) {
    def isMultiple: Boolean = List(twos, fives).min >= multPow
  }

  val factTwosAndFives = (0 to limit).map { n => (n, TwosAndFives(factMaxPow(n, 2), factMaxPow(n, 5))) }.toMap
  val twosAndFives = (0 to limit).map { n => (n, TwosAndFives(maxPow(n, 2), maxPow(n, 5))) }.toMap
  val numeratorTwosAndFives = factTwosAndFives(limit)

  def coefficientInLineCount(tfs: TwosAndFives, b: Int, c: Int): Long = {
    @tailrec
    def go(tfs: TwosAndFives, b: Int, c: Int, acc: Long): Long =
      if (b <= 0) acc + tfs.isMultiple
      else {
        val bTfs = twosAndFives(b)
        val cTfs = twosAndFives(c + 1)
        val updatedTfs = tfs.copy(tfs.twos + bTfs.twos - cTfs.twos, tfs.fives + bTfs.fives - cTfs.fives)

        go(updatedTfs, b - 1, c + 1, acc + tfs.isMultiple)
      }

    go(tfs, b, c, 0)
  }

  val res = (0 to limit).map { a =>
    val size = limit - a + 1
    val aTfs = factTwosAndFives(a)
    val initTfs = TwosAndFives(numeratorTwosAndFives.twos - aTfs.twos, numeratorTwosAndFives.fives - aTfs.fives)
    val halfSize = size / 2
    val cTfs = factTwosAndFives(halfSize)

    if (size % 2 == 0) {
      val bTfs = factTwosAndFives(halfSize - 1)
      2 * coefficientInLineCount(
        TwosAndFives(initTfs.twos - bTfs.twos - cTfs.twos, initTfs.fives - bTfs.fives - cTfs.fives),
        halfSize - 1,
        halfSize
      )
    } else {
      val centerTfs = TwosAndFives(initTfs.twos - 2 * cTfs.twos, initTfs.fives - 2 * cTfs.fives)
      2 * coefficientInLineCount(centerTfs, halfSize, halfSize) - centerTfs.isMultiple
    }
  }.sum

  println(res)
}
