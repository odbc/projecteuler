package ru.odbc.problems.p51to100.p65

object Solution extends App {

  val e: Stream[Int] = 2 #:: 1 #:: Stream.from(1).filter(_ % 2 == 0).flatMap(n => List(n, 1, 1).toStream)

  val ps: Stream[BigInt] = BigInt(1) #:: BigInt(2) #:: ps.zip(ps.tail.zip(e.tail)).map { case (l, (r, ee)) => l + ee * r }

  println(ps.drop(100).head.toString.map(_.asDigit).sum)

}
