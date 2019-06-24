package ru.odbc.problems.p101to150.p105

import scala.io.Source

object Solution extends App {

  val sets = Source.fromResource("p105_sets.txt").getLines
    .map(_.split(",").map(_.toInt).toVector)
    .toVector

  def isSpecial(set: Vector[Int]): Boolean = {
    (for {
      initSize <- 1 to set.size
      initComb <- set.combinations(initSize)
      rest     =  (set.toSet diff initComb.toSet).toVector
      restSize <- 1 to rest.size if restSize >= initSize
      restComb <- rest.combinations(restSize)
    } yield (initComb, restComb))
      .forall { case (l, r) =>
        (l.size == r.size && l.sum != r.sum) || (l.size > r.size && l.sum > r.sum) || (l.size < r.size && l.sum < r.sum)
      }
  }

  println(isSpecial(sets(1)))

  val result = sets.filter(isSpecial).map(_.sum).sum

  println(result)
}
