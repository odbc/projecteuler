package ru.odbc.problems.p52

object Solution extends App {

  def isPermutated[A](left: List[A], right: List[A]): Boolean =
    left match {
      case h :: tail =>
        if (!right.contains(h)) false
        else {
          val index = right.indexOf(h)
          isPermutated(tail, right.take(index) ++ right.drop(index + 1))
        }
      case Nil       => right.isEmpty
    }

  val result = Stream.from(1)
    .map(n => (n.toString, (2 to 6).map(_ * n).map(_.toString)))
    .find { case (n, list) => list.forall(m => isPermutated(n.toList, m.toList)) }

  println(result.get._1)

}
