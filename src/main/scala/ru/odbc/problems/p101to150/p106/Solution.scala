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
    .filter { case (lset, rset) => lset.size == rset.size && lset.size > 1 }
    .filter { case (lset, rset) => lset.last > rset.head }
    .filter { case (lset, rset) =>
      lset.zip(rset).exists { case (l, r) => l > r }
    }

  println(result.size)
}
