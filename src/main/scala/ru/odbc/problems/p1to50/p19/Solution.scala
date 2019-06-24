package ru.odbc.problems.p1to50.p19

object Solution extends App {

  final case class MonthInfo(first: Int, month: Int, year: Int)

  def firstOfMonths(monthInfo: MonthInfo): Stream[MonthInfo] = monthInfo #:: firstOfMonths {
    val nextFirst =
      if (List(1, 3, 5, 7, 8, 10, 12).contains(monthInfo.month)) (monthInfo.first + 31) % 7
      else if (List(4, 6, 9, 11).contains(monthInfo.month)) (monthInfo.first + 30) % 7
      else if (monthInfo.year % 4 == 0) (monthInfo.first + 29) % 7
      else (monthInfo.first + 28) % 7

    monthInfo.copy(
      nextFirst,
      if (monthInfo.month == 12) 1 else monthInfo.month + 1,
      if (monthInfo.month == 12) monthInfo.year + 1 else monthInfo.year
    )
  }

  println(firstOfMonths(MonthInfo(2, 1, 1901)).takeWhile(_.year < 2001).filter(_.first == 0).toList.size)

}
