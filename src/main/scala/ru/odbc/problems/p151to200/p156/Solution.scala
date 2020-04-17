package ru.odbc.problems.p151to200.p156

import lib.mathematics.numberTheory.numbers.Naturals

import scala.annotation.tailrec

object Solution extends App {

  val tenPowers = (0 to 18).map { n => n -> BigInt(10).pow(n).toLong }.toMap

  @tailrec
  def fixPoint(init: Long, f: Long => Long): Long =
    if (init >= tenPowers(17)) Long.MaxValue
    else {
      val next = f(init)
      if (next == init) init
      else fixPoint(next, f)
    }

  def findFix(start: Long, end: Long, f: Long => Long): Vector[Long] =
    if (start > end) Vector(0)
    else {
      val fixStart = fixPoint(start, f)
      val fixEnd = fixPoint(end, f)
      if (fixStart == fixEnd) Vector(fixStart)
      else if (start == fixStart) start +: findFix(start + 1, end, f)
      else if (end == fixEnd) end +: findFix(start, end - 1, f)
      else if (start < fixStart && end > fixEnd) fixStart +: fixEnd +: findFix(fixStart + 1, fixEnd - 1, f)
      else if (start < fixStart && end < fixEnd) fixStart +: findFix(fixStart + 1, end, f)
      else if (start > fixStart && end > fixEnd) fixEnd +: findFix(start, fixEnd - 1, f)
      else findFix(start, (end - start) / 2 + start, f) ++ findFix((end - start) / 2 + start + 1, end, f)
    }

  def f(n: Long, d: Int): Long = {
    val digitsCount = Naturals.digitsCount(n)
    (1 to digitsCount).map { pos =>
      val prefix  = n / tenPowers(digitsCount - pos + 1)
      val digit   = n / tenPowers(digitsCount - pos) % 10
      val postfix = n % tenPowers(digitsCount - pos)

      prefix * tenPowers(digitsCount - pos) + (
        if (digit < d) 0
        else if (digit > d) tenPowers(digitsCount - pos)
        else postfix + 1)
    }.sum
  }

  val res = (1 to 9).map { d =>
    findFix(0L, tenPowers(16), f(_, d)).distinct.filter(_ != Long.MaxValue).sum
  }.sum

  println(res)
}
