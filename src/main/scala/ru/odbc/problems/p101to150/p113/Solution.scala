package ru.odbc.problems.p101to150.p113

object Solution extends App {

  val limitDigits = 100

  val initInc = Map(1 -> (1 to 9).map((_, BigInt(1))).toMap)

  val increasing = (2 to limitDigits).foldLeft(initInc) { case (map, digitCount) =>
    val prevTotal = map(digitCount - 1)
    val nextTotal = (1 to 9).map { digit =>
      (digit, prevTotal.filter(_._1 <= digit).values.sum)
    }.toMap

    map + (digitCount -> nextTotal)
  }.values.map(_.values.sum).sum

  val initDec = Map(1 -> ((0, BigInt(0)) :: (1 to 9).map(n => (n, BigInt(1))).toList).toMap)

  val decreasing = (2 to limitDigits).foldLeft(initDec) { case (map, digitCount) =>
    val prevTotal = map(digitCount - 1)
    val nextTotal = (0 to 9).map { digit =>
      (digit, prevTotal.filter(_._1 >= digit).values.sum)
    }.toMap

    map + (digitCount -> nextTotal)
  }.values.map(_.values.sum).sum - 9 - (limitDigits - 1) * 9

  val result = increasing + decreasing

  println(result)
}
