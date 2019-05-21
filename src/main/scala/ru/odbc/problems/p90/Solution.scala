package ru.odbc.problems.p90

object Solution extends App {

  val arrangements = (0 to 9).combinations(6).map(_.toSet).toList.distinct

  val result = (for {
    left  <- arrangements
    right <- arrangements
    canDisplay01 = (left.contains(0) && right.contains(1)) || (right.contains(0) && left.contains(1))
    canDisplay04 = (left.contains(0) && right.contains(4)) || (right.contains(0) && left.contains(4))
    canDisplay09 =
      (left.contains(0) && (right.contains(9) || right.contains(6))) ||
      (right.contains(0) && (left.contains(9) || left.contains(6)))
    canDisplay16 =
      (left.contains(1) && (right.contains(6) || right.contains(9))) ||
      (right.contains(1) && (left.contains(6) || left.contains(9)))
    canDisplay25 = (left.contains(2) && right.contains(5)) || (right.contains(2) && left.contains(5))
    canDisplay36 =
      (left.contains(3) && (right.contains(6) || right.contains(9))) ||
      (right.contains(3) && (left.contains(6) || left.contains(9)))
    canDisplay49 =
      (left.contains(4) && (right.contains(9) || right.contains(6))) ||
      (right.contains(4) && (left.contains(9) || left.contains(6)))
    canDisplay64 =
      ((left.contains(6) || left.contains(9)) && right.contains(4)) ||
      ((right.contains(6) || right.contains(9)) && left.contains(4))
    canDisplay81 = (left.contains(8) && right.contains(1)) || (right.contains(8) && left.contains(1))
    if canDisplay01 && canDisplay04 && canDisplay09 && canDisplay16 && canDisplay25 &&
       canDisplay36 && canDisplay49 && canDisplay64 && canDisplay81
  } yield Set(left, right)).distinct

  println(result.size)
}
