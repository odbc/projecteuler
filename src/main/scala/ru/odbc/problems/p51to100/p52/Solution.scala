package ru.odbc.problems.p51to100.p52

object Solution extends App {

  def isPermutated[A](left: String, right: String): Boolean = left.sorted == right.sorted

  val (result, _) = Stream.from(1)
    .map(n => (n.toString, (2 to 6).map(_ * n).map(_.toString)))
    .find { case (n, list) => list.forall(m => isPermutated(n, m)) }
    .get

  println(result)
}
