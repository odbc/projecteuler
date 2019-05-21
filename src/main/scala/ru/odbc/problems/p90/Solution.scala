package ru.odbc.problems.p90

object Solution extends App {

  val arrangements = (0 to 9).combinations(6)
    .map(_.toSet)
    .map(s => if (s.contains(6)) s + 9 else if (s.contains(9)) s + 6 else s )
    .toList
    .distinct

  val result = (for {
    left  <- arrangements
    right <- arrangements
    if ((left.contains(0) && right.contains(1)) || (right.contains(0) && left.contains(1))) &&
       ((left.contains(0) && right.contains(4)) || (right.contains(0) && left.contains(4))) &&
       ((left.contains(0) && right.contains(9)) || (right.contains(0) && left.contains(9))) &&
       ((left.contains(1) && right.contains(6)) || (right.contains(1) && left.contains(6))) &&
       ((left.contains(2) && right.contains(5)) || (right.contains(2) && left.contains(5))) &&
       ((left.contains(3) && right.contains(6)) || (right.contains(3) && left.contains(6))) &&
       ((left.contains(4) && right.contains(9)) || (right.contains(4) && left.contains(9))) &&
       ((left.contains(6) && right.contains(4)) || (right.contains(6) && left.contains(4))) &&
       ((left.contains(8) && right.contains(1)) || (right.contains(8) && left.contains(1)))
  } yield Set(left, right)).distinct

  println(result.size)

}
