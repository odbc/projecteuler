package ru.odbc.problems.p101to150.p106

object Solution extends App {

  val limit = 12

  val indices  = (0 until limit).toVector

  val combPairs = (for {
    initSize <- (1 until limit).toVector
    restSize <- initSize until limit
    initComb <- indices.combinations(initSize)
    rest = (indices.toSet diff initComb.toSet).toVector.sorted
    restComb <- rest.combinations(restSize)
  } yield Set(initComb.toSet, restComb.toSet)).distinct.map(_.map(_.toVector).toVector).map(v => (v(0).sorted, v(1).sorted))

  val result = combPairs
    .filter { case (lSet, rSet) => lSet.size == rSet.size && lSet.size > 1 }
    .filter { case (lSet, rSet) => lSet.last > rSet.head }
    .count  { case (lSet, rSet) => lSet.zip(rSet).exists { case (l, r) => l > r } }

  println(result)
}
