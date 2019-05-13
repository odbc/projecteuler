package ru.odbc.problems.p51

import commons.primes.{primes, isPrime}

object Solution extends App {

  val sets = "0123456789".combinations(8)
    .filter(s => s.startsWith("0") || s.startsWith("1") || s.startsWith("2")).toList
    .groupBy(_(0))

  def findIndicesOfChar(str: String, char: Char): List[Int] =
    str.zipWithIndex.foldRight(List.empty[Int]) { case ((c, i), acc) => if (c == char) i :: acc else acc }

  val result = primes
    .map { p =>
      p -> List('0', '1', '2').map(c => c -> findIndicesOfChar(p.toString, c)).filter(_._2.size > 2).toMap
    }
    .filter(_._2.nonEmpty)
    .map { case (p, m) =>
      (p, m.map { case (char, li) =>
        val replaced = for {
          positions  <- (2 to li.length).flatMap(li.combinations)
          replaceSet <- sets.getOrElse(char, List())
        } yield replaceSet.map(c => BigInt(positions.foldLeft(p.toString)((str, pos) => str.updated(pos, c))))

        (char, replaced)
      }.values.flatten)
    }
    .find { case (_, l) => l.exists(set => set.forall(isPrime)) }

  println(result.get)

}
