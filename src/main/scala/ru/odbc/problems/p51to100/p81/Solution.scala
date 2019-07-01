package ru.odbc.problems.p51to100.p81

import lib.algorithms.graphs.{Edge, FordBellman, Graph}

import scala.io.Source

object Solution extends App {

  def numByRowAndIndex(row: Int, index: Int): Int = row * limit + index

  val limit = 80
  val total = limit * limit

  val rows = Source.fromResource("p081_matrix.txt").getLines
    .map(_.split(",").map(_.toInt).toVector).toVector

  val graph = rows.zipWithIndex.foldLeft(Graph()) { case (accRows, (row, rowIndex)) =>
    row.zipWithIndex.foldLeft(accRows) { case (accEls, (_, elIndex)) =>
      if (rowIndex == limit - 1 && elIndex == limit - 1) accEls
      else {
        val source = numByRowAndIndex(rowIndex, elIndex)
        if (rowIndex == limit - 1)
          accEls :+ Edge(source, numByRowAndIndex(rowIndex, elIndex + 1), rows(rowIndex)(elIndex + 1))
        else if (elIndex == limit - 1)
          accEls :+ Edge(source, numByRowAndIndex(rowIndex + 1, elIndex), rows(rowIndex + 1)(elIndex))
        else
          accEls :+
            Edge(source, numByRowAndIndex(rowIndex, elIndex + 1), rows(rowIndex)(elIndex + 1)) :+
            Edge(source, numByRowAndIndex(rowIndex + 1, elIndex), rows(rowIndex + 1)(elIndex))
      }
    }
  }

  val result = FordBellman(graph).minFrom(0).last + rows(0)(0)

  println(result)
}
