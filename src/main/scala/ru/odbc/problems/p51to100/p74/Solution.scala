package ru.odbc.problems.p51to100.p74

object Solution extends App {

  val factorials = ((0, 1) :: (1 to 9).map(n => (n, (1 to n).product)).toList).toMap

  def chain(start: BigInt): List[BigInt] = {
    def go(list: List[BigInt]): List[BigInt] = {
      val next = list.head.toString.map(c => BigInt(factorials(c.asDigit))).sum
      if (list.contains(next)) list.reverse
      else go(next :: list)
    }
    go(start :: Nil)
  }

  val result = (1 until 1000000).map(chain(_)).count(l => l.size == 60)

  println(result)
}
