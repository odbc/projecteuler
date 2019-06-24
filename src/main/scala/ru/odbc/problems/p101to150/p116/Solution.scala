package ru.odbc.problems.p101to150.p116

object Solution extends App {

  def ways(segSize: Int, rowSize: Int): BigInt = {
    val initWays = (1 until segSize).map((_, BigInt(0))).toMap + (segSize -> BigInt(1))
    val ways = (segSize + 1 to rowSize).foldLeft(initWays) { case (ws, rowLen) =>
      val newWays = ws(rowLen - 1) + ws(rowLen - segSize) + 1
      ws + (rowLen -> newWays)
    }

    ways(rowSize)
  }

  val rowSize = 50
  val result  = ways(2, rowSize) + ways(3, rowSize) + ways(4, rowSize)

  println(result)
}
