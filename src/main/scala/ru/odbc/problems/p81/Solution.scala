package ru.odbc.problems.p81

import scala.io.Source

object Solution extends App {

  final case class Edge(start: Int, end: Int, cost: Int)

  val limit = 80
  val total = limit * limit

  val rows = Source.fromResource("p081_matrix.txt").getLines
    .map(_.split(",").map(_.toInt).toVector).toVector

  def numByRowAndIndex(row: Int, index: Int): Int = row * limit + index

  val graph = for {
    (row, ri) <- rows.zipWithIndex
    (_, i)    <- row.zipWithIndex
    rightDown =
      if (ri == limit - 1 && i == limit - 1) Vector()
      else if (ri == limit - 1) Vector(Edge(numByRowAndIndex(ri, i), numByRowAndIndex(ri, i + 1), rows(ri)(i + 1)))
      else if (i == limit - 1) Vector(Edge(numByRowAndIndex(ri, i), numByRowAndIndex(ri + 1, i), rows(ri + 1)(i)))
      else Vector(
        Edge(numByRowAndIndex(ri, i), numByRowAndIndex(ri, i + 1), rows(ri)(i + 1)),             // to right
        Edge(numByRowAndIndex(ri, i), numByRowAndIndex(ri + 1, i), rows(ri + 1)(i)),             // to down
      )
    edge      <- rightDown
  } yield edge

  println(graph)

  /** Bellman–Ford algorithm
    * http://e-maxx.ru/algo/ford_bellman
    */
  val paths = Array(0) ++ Array.fill(total - 1)(Int.MaxValue)

  for {
    _ <- 0 until paths.length - 1
    j <- graph.indices if paths(graph(j).start) < Int.MaxValue
  } paths(graph(j).end) = List(paths(graph(j).end), paths(graph(j).start) + graph(j).cost).min

  println(paths.last + rows(0)(0))

}
