package ru.odbc.problems.p93

import commons.operations.gcd

object Solution extends App {

  case class Ratio(num: Int, den: Int) {
    def +(other: Ratio) = Ratio(this.num * other.den + this.den * other.num, this.den * other.den)
    def -(other: Ratio) = Ratio(this.num * other.den - this.den * other.num, this.den * other.den)
    def *(other: Ratio) = Ratio(this.num * other.num, this.den * other.den)
    def /(other: Ratio) = Ratio(this.num * other.den, this.den * other.num)
  }

  object Ratio {
    def apply(num: Int, den: Int): Ratio = {
      val g = gcd(num, den).toInt
      val n = num / g
      val d = den / g
      if (d >= 0) new Ratio(n, d)
      else new Ratio(-n, -d)
    }
  }

  val consecutives = (0 to 9).combinations(4).map { digitSet =>
    (digitSet, digitSet.map(Ratio(_, 1)).permutations.flatMap { perm =>
      perm.tail.foldLeft(List(perm.head)) { case (acc, d) =>
        acc.flatMap { e =>
          val add = List(e + d)
          val mul = List(e * d)
          val sub = List(e - d, d - e)
          val div =
            if (d.num != 0 && e.num != 0) List(d / e, e / d)
            else if (d.num != 0) List(e / d)
            else if (e.num != 0) List(d / e)
            else List()

          add ++ mul ++ sub ++ div
        }
      }.filter(r => r.num > 0 && r.den == 1 ).map(_.num)
    }.toList.distinct.sorted)
  }

  val result = consecutives.map { case (digitSet, ints) =>
    (
      digitSet,
      ints.sliding(2, 1).foldLeft(0) { case (m, pair) => if (m == 0 && pair.last - pair.head > 1) pair.head else m }
    )
  }.maxBy(_._2)._1

  println(result.sorted.mkString)

}
