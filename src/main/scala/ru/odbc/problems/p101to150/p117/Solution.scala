package ru.odbc.problems.p101to150.p117

object Solution extends App {

  def ways(rowSize: Int): BigInt = {
    val initWays = Map(0 -> BigInt(1), 1 -> BigInt(1), 2 -> BigInt(2), 3 -> BigInt(4))
    val ways = (4 to rowSize).foldLeft(initWays) { case (ws, rowLen) =>
      val newWays = ws(rowLen - 1) + ws(rowLen - 2) + ws(rowLen - 3) + ws(rowLen - 4)
      ws + (rowLen -> newWays)
    }

    ways(rowSize)
  }

  val rowSize = 50
  val result  = ways(rowSize)

  println(result)

}
