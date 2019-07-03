package ru.odbc.problems.p101to150.p114

object Solution extends App {

  val ways = Map(0 -> BigInt(1))

  val rowLength  = 50
  val segmentMin = 3

  val result = (1 to rowLength).foldLeft(ways) { case (ws, rowLen) =>
    if (rowLen < segmentMin) ws + (rowLen -> BigInt(1))
    else if (rowLen == segmentMin) ws + (rowLen -> BigInt(2))
    else {
      val newWays = ws(rowLen - 1) + (segmentMin until rowLen).map(l => ws(rowLen - l - 1)).sum + 1
      ws + (rowLen -> newWays)
    }
  }(rowLength)

  println(result)
}
