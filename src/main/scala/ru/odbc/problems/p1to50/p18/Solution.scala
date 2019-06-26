package ru.odbc.problems.p1to50.p18

import lib.algorithms.graphs.{Edge, FordBellman, Graph}

object Solution extends App {

  def numByRowAndIndex(row: Int, index: Int): Int = row * (row + 1) / 2 + index + 1

  val inputStr = "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  val input = inputStr.split(" ").map(_.toInt).toVector

  val limit = 15
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
