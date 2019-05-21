package ru.odbc.problems.p92

object Solution extends App {

  def chain(from: Int): Stream[Int] = from #:: chain(from.toString.map(_.asDigit).map(d => d * d).sum)

  val limit = 10000000

  val result= (1 to limit).count { n =>
    chain(n).dropWhile(e => e != 1 && e != 89).head == 89
  }

  println(result)
}
