package ru.odbc.problems.p122

import commons.algorithms.whileLoop

object Solution extends App {

  val limit = 200

  val (initStep, initMap, initVs) = whileLoop[(Int, Map[Int, Int], Vector[Vector[Int]])] { case (step, map, _) =>
    map.values.exists(_ == 0) && step < 11
  } { case (step, map, vs) =>
    val nextVs = vs.flatMap { v =>
      val withOne = if (v.last + 1 <= limit) Vector(v :+ (v.last + 1)) else Vector()
      val withTwo = if (v.last + 2 <= limit) Vector(v :+ (v.last + 2)) else Vector()
      withOne ++ withTwo ++ v.filter(v.last + _ <= limit).map(n => v :+ (v.last + n))
    }
    val nextMap = nextVs.foldLeft(map) { case (m, v) =>
      if (m(v.last) == 0) m.updated(v.last, step) else m
    }
    (step + 1, nextMap, nextVs)
  } { (3, (5 to limit).map((_, 0)).toMap + (1 -> -1) + (2 -> 1) + (3 -> 2) + (4 -> 2), Vector(Vector(3), Vector(4))) }

  val result = initMap ++ initVs.foldLeft(initMap.filter(_._2 == 0)) { case (map, v) =>
    (1 +: 2 +: v).foldLeft(map) { case (m, e) =>
      val n = v.last + e
      if (m.contains(n) && m(n) == 0) m.updated(n, initStep) else m
    }
  }

  println(result.values.sum + 1)
}
