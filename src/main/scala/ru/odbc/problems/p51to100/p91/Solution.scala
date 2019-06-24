package ru.odbc.problems.p51to100.p91

object Solution extends App {

  val limit = 50

  val result = (for {
    x1 <- 0 to limit
    y1 <- 0 to limit
    x2 <- 0 to limit
    y2 <- 0 to limit
    side1sq = x1 * x1 + y1 * y1
    side2sq = x2 * x2 + y2 * y2
    side3sq = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
    if side1sq != 0 && side2sq != 0 && side3sq != 0 &&
       ((side1sq + side2sq == side3sq) || (side1sq + side3sq == side2sq) || (side2sq + side3sq == side1sq))
  } yield 1).sum / 2

  println(result)
}
