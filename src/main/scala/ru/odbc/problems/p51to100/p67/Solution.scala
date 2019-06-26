package ru.odbc.problems.p51to100.p67

import lib.algorithms.graphs.{Edge, FordBellman, Graph}

import scala.io.Source

object Solution extends App {

  def numByRowAndIndex(row: Int, index: Int): Int = row * (row + 1) / 2 + index + 1

  val input = Source.fromResource("p067_triangle.txt").getLines.flatMap(_.split(" ").map(_.toInt)).toVector

  val limit = 100
  val total = limit * (limit + 1) / 2

  val (rows, _) = (1 to limit).foldLeft((Vector[Vector[Int]](), input)) { case ((out, in), i) =>
    (out ++ Vector(in.take(i)), in.drop(i))
  }

  val graph = Vector.range(total - limit + 1, total + 1).foldLeft {
    rows.zipWithIndex.foldLeft(Graph()) { case (accRows, (row, rowIndex)) =>
      if (rowIndex == 0) accRows :+ Edge(0, 1, -row.head)
      else
        row.zipWithIndex.foldLeft(accRows) { case (accEls, (el, elIndex)) =>
          val target = numByRowAndIndex(rowIndex, elIndex)
          if (elIndex == 0) {
            val source = numByRowAndIndex(rowIndex - 1, elIndex)
            accEls :+ Edge(source, target, -el)
          } else if (elIndex == row.length - 1) {
            val source = numByRowAndIndex(rowIndex - 1, elIndex - 1)
            accEls :+ Edge(source, target, -el)
          } else {
            val (sourceL, sourceR) = (numByRowAndIndex(rowIndex - 1, elIndex - 1), numByRowAndIndex(rowIndex - 1, elIndex))
            accEls :+ Edge(sourceL, target, -el) :+ Edge(sourceR, target, -el)
          }
        }
    }
  } { case (acc, v) => acc :+ Edge(v, total + 1)}

  val result = -FordBellman(graph).minFrom(0).last

  println(result)
}
