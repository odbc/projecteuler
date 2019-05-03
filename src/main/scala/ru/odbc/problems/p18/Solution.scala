package ru.odbc.problems.p18

object Solution extends App {

  final case class Edge(start: Int, end: Int, cost: Int)

  val inputStr = "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  val input = inputStr.split(" ").map(_.toInt).toVector

  val limit = 15
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
    j <- 0 until graph.size - 1 if paths(graph(j).start) < Int.MaxValue
  } paths(graph(j).end) = List(paths(graph(j).end), paths(graph(j).start) + graph(j).cost).min

  println(-paths.last)
}
