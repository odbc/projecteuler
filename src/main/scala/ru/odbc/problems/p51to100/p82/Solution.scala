package ru.odbc.problems.p51to100.p82

import lib.algorithms.graphs.{Edge, FordBellman, Graph}

import scala.io.Source

object Solution extends App {

  def numByRowAndIndex(row: Int, index: Int): Int = row * limit + index + 1

  val limit = 80
  val total = limit * limit

  val rows = Source.fromResource("p082_matrix.txt").getLines
    .map(_.split(",").map(_.toInt).toVector).toVector

  val graph = Vector.range(0, limit).foldLeft {
    rows.zipWithIndex.foldLeft(Graph()) { case (accRows, (row, rowIndex)) =>
      row.zipWithIndex.foldLeft(accRows) { case (accEls, (el, elIndex)) =>
        val withBorder =
          if (elIndex % limit == 0) accEls :+ Edge(0, numByRowAndIndex(rowIndex, elIndex), el) else accEls

        val withRightDown =
          if (rowIndex == limit - 1 && elIndex == limit - 1) withBorder
          else {
            val source = numByRowAndIndex(rowIndex, elIndex)
            if (rowIndex == limit - 1)
              withBorder :+ Edge(source, numByRowAndIndex(rowIndex, elIndex + 1), rows(rowIndex)(elIndex + 1))
            else if (elIndex == limit - 1)
              withBorder :+ Edge(source, numByRowAndIndex(rowIndex + 1, elIndex), rows(rowIndex + 1)(elIndex))
            else withBorder :+
              Edge(source, numByRowAndIndex(rowIndex, elIndex + 1), rows(rowIndex)(elIndex + 1)) :+
              Edge(source, numByRowAndIndex(rowIndex + 1, elIndex), rows(rowIndex + 1)(elIndex))
          }

        if (rowIndex == 0) withRightDown
        else withRightDown :+ Edge(numByRowAndIndex(rowIndex, elIndex), numByRowAndIndex(rowIndex - 1, elIndex), rows(rowIndex - 1)(elIndex))
      }
    }
  } { case (acc, r) => acc :+ Edge(numByRowAndIndex(r, limit - 1), total + 1)}

  val result = FordBellman(graph).minFrom(0).last

  println(result)
}
