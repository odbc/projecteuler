package ru.odbc.problems.p102

import scala.io.Source

object Solution extends App {

  case class Point(x: BigInt, y: BigInt)

  case class Triangle(a: Point, b: Point, c: Point)

  def sign(p1: Point, p2: Point, p3: Point): Int = {
    val s = (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)
    if (s > 0) 1 else if (s < 0) -1 else 0
  }

  val triangles = Source.fromResource("p102_triangles.txt").getLines
    .map(_.split(",").map(BigInt(_)))
    .map(cs => Triangle(Point(cs(0), cs(1)), Point(cs(2), cs(3)), Point(cs(4), cs(5))))
    .toList

  val result = triangles.count { tr =>
    val origin = Point(0, 0)

    val d1 = sign(origin, tr.a, tr.b)
    val d2 = sign(origin, tr.b, tr.c)
    val d3 = sign(origin, tr.c, tr.a)

    val has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0)
    val has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0)

    !(has_neg && has_pos)
  }

  println(result)
}
