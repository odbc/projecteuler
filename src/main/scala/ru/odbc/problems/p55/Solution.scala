package ru.odbc.problems.p55

object Solution extends App {

  def lychrels(start: BigInt): Stream[BigInt] = start #:: lychrels(start + BigInt(start.toString.reverse))

  val result = (1 to 10000).filter(lychrels(_).tail.take(50).forall(n => n.toString != n.toString.reverse))

  println(result.size)

}
