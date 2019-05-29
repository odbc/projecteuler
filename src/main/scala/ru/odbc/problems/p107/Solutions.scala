package ru.odbc.problems.p107

import scala.io.Source

object Solutions extends App {

  val network = Source.fromResource("p107_network.txt")
    .getLines.map(_.split(",").map(e => if (e == "-") Int.MaxValue else e.toInt).toVector).toVector

  val total = network.map(_.map(e => if (e == Int.MaxValue) 0 else e).sum).sum / 2

  /**
    * Prim's algorithm that finds a minimum spanning tree for a weighted undirected graph
    * https://en.wikipedia.org/wiki/Prim's_algorithm
    * http://e-maxx.ru/algo/mst_prim
    */
  def prim(graph: Vector[Vector[Int]]): Vector[Int] = {
    val n = graph.size

    def go(acc: Vector[Int], used: Vector[Boolean], minEdge: Vector[Int], selEdge: Vector[Int]): Vector[Int] = {
      if (acc.size == n - 1) acc
      else {
        val v = (0 until n).filter(!used(_)).minBy(minEdge(_))

        if (minEdge(v) == Int.MaxValue) Vector()
        else {
          val newAcc = if (selEdge(v) != Int.MinValue) graph(v)(selEdge(v)) +: acc else acc
          val newUsed = used.updated(v, true)
          val (newMin, newSel) = (0 until n)
            .filter(to => graph(v)(to) < minEdge(to))
            .foldLeft((minEdge, selEdge)) { case ((me, se), to) =>
              (me.updated(to, graph(v)(to)), se.updated(to, v))
            }

          go(newAcc, newUsed, newMin, newSel)
        }
      }
    }
    go(Vector.empty, Vector.fill(n)(false), Vector(0) ++ Vector.fill(n - 1)(Int.MaxValue), Vector.fill(n)(Int.MinValue))
  }

  val result = total - prim(network).sum

  println(result)
}
