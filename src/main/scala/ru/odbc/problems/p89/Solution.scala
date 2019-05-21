package ru.odbc.problems.p89

import scala.io.Source

object Solution extends App {

  val numerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  def romanToInt: String => Int = { r =>
    val repr = r.map(numerals.getOrElse(_, 0))

    if (repr.isEmpty) 0
    else if (repr.size == 1) repr.head
    else
      repr.sliding(2)
        .foldRight(0) { case (pair, acc) =>
          if (pair(0) >= pair(1)) acc + pair(1)
          else acc + pair(1) - 2 * pair(0)
        } + repr.head
  }

  def intToRoman: Int => String = { n =>
    if (n >= 1000) "M" + intToRoman(n - 1000)
    else if (n >= 900) "CM" + intToRoman(n - 900)
    else if (n >= 500) "D"  + intToRoman(n - 500)
    else if (n >= 400) "СD" + intToRoman(n - 400)
    else if (n >= 100) "С"  + intToRoman(n - 100)
    else if (n >=  90) "XС" + intToRoman(n -  90)
    else if (n >=  50) "L"  + intToRoman(n -  50)
    else if (n >=  40) "XL" + intToRoman(n -  40)
    else if (n >=  10) "X"  + intToRoman(n -  10)
    else if (n >=   9) "IX" + intToRoman(n -   9)
    else if (n >=   5) "V"  + intToRoman(n -   5)
    else if (n >=   4) "IV" + intToRoman(n -   4)
    else if (n >=   1) "I"  + intToRoman(n -   1)
    else ""
  }

  val result = Source.fromResource("p089_roman.txt").getLines
    .map(r => r.length - (intToRoman compose romanToInt)(r).length)
    .sum

  println(result)
}
