package ru.odbc.problems.p62

object Solution extends App {

  def isPermutated(l: String, r: String): Boolean =
    if (l.length != r.length) false
    else l.sorted.zip(r.sorted).forall { case (lc, rc) => lc == rc }

  val cubesByLength = (BigInt(1) to BigInt(10000)).map(n => n * n * n).groupBy(_.toString.length)

  def findGroup(list: List[BigInt]): List[List[BigInt]] =
    for {
      s1 <- list
      s2 <- list if s2 > s1 && isPermutated(s2.toString, s1.toString)
      s3 <- list if s3 > s2 && isPermutated(s3.toString, s2.toString)
      s4 <- list if s4 > s3 && isPermutated(s4.toString, s3.toString)
      s5 <- list if s5 > s4 && isPermutated(s5.toString, s4.toString)
    } yield List(s1, s2, s3, s4, s5)

  println(cubesByLength.mapValues(l => findGroup(l.toList)).filter(_._2.nonEmpty))
}
