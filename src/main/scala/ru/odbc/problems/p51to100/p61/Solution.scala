package ru.odbc.problems.p51to100.p61

object Solution extends App {

  val polygonals = Map(
    3 -> Stream.from(1).map(n => n * (n + 1) / 2).take(150).filter(n => n.toString.length == 4).toList,
    4 -> Stream.from(1).map(n => n * n).take(100).filter(n => n.toString.length == 4).toList,
    5 -> Stream.from(1).map(n => n * (3 * n - 1) / 2).take(100).filter(n => n.toString.length == 4).toList,
    6 -> Stream.from(1).map(n => n * (2 * n - 1)).take(100).filter(n => n.toString.length == 4).toList,
    7 -> Stream.from(1).map(n => n * (5 * n - 3) / 2).take(100).filter(n => n.toString.length == 4).toList,
    8 -> Stream.from(1).map(n => n * (3 * n - 2)).take(100).filter(n => n.toString.length == 4).toList
  )

  val polygonalTypes = Set(3, 4, 5, 6, 7, 8)

  val notOctaPairs = polygonals.map { case (t, l) => l.map((t, _)) }.flatten.filter(_._1 != 8)
  val octaSets = for {
    o <- polygonals(8)
    (prevType, prev) <- notOctaPairs.filter { case (_, n) => n.toString.drop(2) == o.toString.take(2) }
    (nextType, next) <- notOctaPairs.filter { case (_, n) => n.toString.take(2) == o.toString.drop(2) }
    if prevType != nextType
    (prevPrevType, prevPrev) <- notOctaPairs.filter { case (_, n) => n.toString.drop(2) == prev.toString.take(2) }
    (nextNextType, nextNext) <- notOctaPairs.filter { case (_, n) => n.toString.take(2) == next.toString.drop(2) }
    if prevPrevType != prevType && prevPrevType != nextType &&
       nextNextType != prevType && nextNextType != nextType &&
       prevPrevType != nextNextType
  } yield (List(prevPrevType, prevType, 8, nextType, nextNextType), List(prevPrev, prev, o, next, nextNext))

  val sets = octaSets.filter { case (types, set) =>
    val t = polygonalTypes.diff(types.toSet).head
    polygonals(t).contains((set.last.toString.drop(2) + set.head.toString.take(2)).toLong)
  }

  val resultSet = sets.head._2
  val result = ((resultSet.last.toString.drop(2) + resultSet.head.toString.take(2)).toInt :: resultSet).sum

  println(result)
}
