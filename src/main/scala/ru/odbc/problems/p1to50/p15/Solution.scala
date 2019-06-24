package ru.odbc.problems.p1to50.p15

import scala.collection.mutable

object Solution extends App {

  val cache = mutable.Map.empty[(Int, Int), Long]

  def routes(x: Int, y: Int): Long =
    cache.getOrElseUpdate(
      (x, y),
      if (x == 0 && y == 0) 0L
      else if (x == 0) 1L
      else if (y == 0) 1L
      else routes(x - 1, y) + routes(x, y - 1)
    )

  println(routes(20, 20))

}
