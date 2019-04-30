package ru.odbc.problems.p4

object Solution extends App {

  val xs = 100 to 999

  val result = (for { x <- xs; y <- xs } yield x * y).filter(p => { val str = p.toString; str == str.reverse }).max

  println(result)

}
