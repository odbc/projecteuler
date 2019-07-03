package ru.odbc.problems.p101to150.p103

object Solution extends App {

  val limit = 7
  val init  = (1 to limit).toVector

  val pairCombs = (2 until limit).map { n =>
    (n, (0 until n).combinations(2).toList)
  }.toMap

  def setsBySum(sum: Int): Vector[Vector[Int]] = {
    val pairSum = sum - (limit - 2)
    val pairs = for {
      a <- (1 until pairSum).toVector
      b <- a + 1 to pairSum - a
    } yield Vector(a, b)

    (1 to limit - 3).foldLeft(pairs) { case (acc, _) =>
      acc.flatMap { set =>
        val maxNext = Vector(sum - set.sum - set.last, set(0) + set(1)).min
        ((set.last + 1) to maxNext)
          .filter { n =>
            set.map(_ + n).forall(!pairCombs(set.size).map(c => set(c(0)) + set(c(1))).contains(_))
          }
          .map(set ++ Vector(_))
      }
    }.filter { set =>
      val rest = sum - set.sum
      rest > set.last && rest < set(0) + set(1) &&
        set.map(_ + rest).forall(!pairCombs(set.size).map(c => set(c(0)) + set(c(1))).contains(_))
    }.map(set => set ++ Vector(sum - set.sum))
  }

  val indices  = (0 until limit).toVector

  val combPairs = (for {
    initSize <- 1 to limit
    initComb <- indices.combinations(initSize)
    rest     =  (indices.toSet diff initComb.toSet).toVector
    restSize <- 1 to rest.size if restSize >= initSize
    restComb <- rest.combinations(restSize)
  } yield Set(initComb.toSet, restComb.toSet)).distinct.toVector
    .map((s: Set[Set[Int]]) => { val v = s.toVector; (v(0), v(1)) })
    .filter { case (l, r) => l.size != 1 || r.size != 1 }
    .filter { case (l, r) => l.size != 2 || r.size != 2 }
    .filter { case (l, r) => !((l.size < r.size && l.last < r.head) || (l.size > r.size && l.last > r.head)) }

  val result = Stream.from(1).map(n => (n, setsBySum(n)))
    .dropWhile {case (_, sets) =>
      sets.forall { set =>
        combPairs.exists { case (lInit, rInit) =>
          val l = lInit.map(set(_))
          val r = rInit.map(set(_))
          (l.size == r.size && l.sum == r.sum) || (l.size > r.size && l.sum <= r.sum) || (l.size < r.size && l.sum >= r.sum)
        }
      }
    }.head._2
    .find { set =>
      combPairs.forall { case (lInit, rInit) =>
        val l = lInit.map(set(_))
        val r = rInit.map(set(_))
        (l.size == r.size && l.sum != r.sum) || (l.size > r.size && l.sum > r.sum) || (l.size < r.size && l.sum < r.sum)
      }
    }.get.mkString

  println(result)
}
