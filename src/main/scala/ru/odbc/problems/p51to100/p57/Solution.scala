package ru.odbc.problems.p51to100.p57

object Solution extends App {

  val ps: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: ps.zip(ps.tail).map { case (l, r) => l + 2 * r }
  val qs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: qs.zip(qs.tail).map { case (l, r) => l + 2 * r }

  val result = ps.zip(qs).slice(2, 1002).count { case (p, q) => p.toString.length > q.toString.length }

  println(result)
}
