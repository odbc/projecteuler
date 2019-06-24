package ru.odbc.problems.p51to100.p68

object Solution extends App {

  val result = for {
    perm <- (1 to 10).toVector.permutations
    group1 = List(perm(0), perm(1), perm(2))
    group2 = List(perm(3), perm(2), perm(4))
    group3 = List(perm(5), perm(4), perm(6))
    group4 = List(perm(7), perm(6), perm(8))
    group5 = List(perm(9), perm(8), perm(1))
    groups = List(group1, group2, group3, group4, group5)
    if groups.minBy(_.head).head == groups.head.head && groups.tail.forall(_.sum == groups.head.sum)
  } yield groups.flatten.mkString

  println(result.filter(_.length == 16).map(BigInt(_)).max)

}
