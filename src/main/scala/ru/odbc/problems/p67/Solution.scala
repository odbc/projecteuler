package ru.odbc.problems.p67

import scala.io.Source

object Solution extends App {

  final case class Edge(start: Int, end: Int, cost: Int)

  val input = Source.fromResource("p067_triangle.txt").getLines.flatMap(_.split(" ").map(_.toInt)).toVector

  val limit = 100
  val total = limit * (limit + 1) / 2

  val rows = (1 to limit).foldLeft((Vector[Vector[Int]](), input)) { case ((out, in), i) =>
    (out ++ Vector(in.take(i)), in.drop(i))
  }._1

  def numByRowAndIndex(row: Int, index: Int): Int = row * (row + 1) / 2 + index + 1

  val graph = (for {
    (row, ri) <- rows.zipWithIndex
    (e, i)    <- row.zipWithIndex
    edge     <- {
      if (ri - 1 < 0) Vector(Edge(0, 1, -e))
      else {
        if (i == 0) Vector(Edge(numByRowAndIndex(ri - 1, i), numByRowAndIndex(ri, i), -e))
        else if (i >= row.length - 1) Vector(Edge(numByRowAndIndex(ri - 1, i - 1), numByRowAndIndex(ri, i), -e))
        else Vector(
          Edge(numByRowAndIndex(ri - 1, i - 1), numByRowAndIndex(ri, i), -e),
          Edge(numByRowAndIndex(ri - 1, i), numByRowAndIndex(ri, i), -e)
        )
      }
    }
  } yield edge) ++ Vector.range(total - limit + 1, total + 1).map(Edge(_, total + 1, 0))

  /** Bellman–Ford algorithm
    * http://e-maxx.ru/algo/ford_bellman
    */
  val paths = Array(0) ++ Array.fill(total + 1)(Int.MaxValue)

  for {
    _ <- 0 until paths.length - 1
    j <- graph.indices if paths(graph(j).start) < Int.MaxValue
  } paths(graph(j).end) = List(paths(graph(j).end), paths(graph(j).start) + graph(j).cost).min

  println(-paths.last)

}
