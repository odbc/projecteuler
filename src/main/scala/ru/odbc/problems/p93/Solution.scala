package ru.odbc.problems.p93

import commons.numbers.Ratio

object Solution extends App {

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
      ints.sliding(2, 1).foldLeft(BigInt(0)) { case (m, pair) => if (m == 0 && pair.last - pair.head > 1) pair.head else m }
    )
  }.maxBy(_._2)._1

  println(result.sorted.mkString)

}
