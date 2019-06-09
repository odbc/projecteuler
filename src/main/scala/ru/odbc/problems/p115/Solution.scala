package ru.odbc.problems.p115

object Solution extends App {

  val segmentMin = 50
  val limit = 1000000

  def ways(rowLength: Int): BigInt = {
    val initWays = Map(0 -> BigInt(1))
    val ways = (1 to rowLength).foldLeft(initWays) { case (ws, rowLen) =>
      if (rowLen < segmentMin) ws + (rowLen -> BigInt(1))
      else if (rowLen == segmentMin) ws + (rowLen -> BigInt(2))
      else {
        val newWays = ws(rowLen - 1) + (segmentMin until rowLen).map(l => ws(rowLen - l - 1)).sum + 1
        ws + (rowLen -> newWays)
      }
    }

    ways(rowLength)
  }

  val result = Stream.from(1).dropWhile(ways(_) <= limit).head

  println(result)
}
