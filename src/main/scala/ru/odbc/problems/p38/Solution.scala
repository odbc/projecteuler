package ru.odbc.problems.p38

object Solution extends App {

  val perms = (1 to 9).permutations./*slice(35377, 35378).*/map(_.mkString).filter { perm =>
    val prefixes = List.iterate(perm.init, perm.length - 1)(_.init)
    val ps = prefixes map { prefix =>
      val ms = (1 to 9).map(_ * prefix.toInt).map(_.toString)
        .foldLeft(Option[String]("")) { (acc, str) =>
          acc match {
            case Some(s) =>
              if (s == perm) Some(perm)
              else {
                val newAcc = s + str
                if (newAcc.length <= 9) Some(newAcc)
                else None
              }
            case _ => None
          }
        }
      ms
    } filter { s => s.isDefined && s.get == perm }

    ps.nonEmpty
  }

  println(perms.toList)

}
