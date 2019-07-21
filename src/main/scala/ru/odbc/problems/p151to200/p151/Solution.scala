package ru.odbc.problems.p151to200.p151

object Solution extends App {

  val batches = 16

  val init = Vector((0, 1.0, List(1, 1, 1, 1)))

  val ways = (2 until batches).foldLeft(init) { case (v, _) =>
    v.flatMap { case (count, prob, sheets) =>
      val total = sheets.sum
      val ps = sheets.zipWithIndex.filter(_._1 > 0)
      ps.map { case (e, i) =>
        (
          if (ps.length == 1 && ps.head._1 == 1) count + 1 else count,
          prob * e / total,
          (sheets.slice(0, i) :+ e - 1) ++ sheets.slice(i + 1, sheets.length).map(_ + 1)
        )
      }
    }
  }

  val exp = ways.groupBy(_._1).mapValues(_.map(_._2).sum).foldLeft(0.0) { case (acc, (t, p)) => t * p + acc }
  val result = BigDecimal(exp).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble

  println(result)
}
