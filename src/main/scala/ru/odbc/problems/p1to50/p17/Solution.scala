package ru.odbc.problems.p1to50.p17

object Solution extends App {

  val digits = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  val belowHundred = digits ++ List("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twentyone", "twentytwo", "twentythree", "twentyfour", "twentyfive", "twentysix", "twentyseven", "twentyeight", "twentynine", "thirty", "thirtyone", "thirtytwo", "thirtythree", "thirtyfour", "thirtyfive", "thirtysix", "thirtyseven", "thirtyeight", "thirtynine", "forty", "fortyone", "fortytwo", "fortythree", "fortyfour", "fortyfive", "fortysix", "fortyseven", "fortyeight", "fortynine", "fifty", "fiftyone", "fiftytwo", "fiftythree", "fiftyfour", "fiftyfive", "fiftysix", "fiftyseven", "fiftyeight", "fiftynine", "sixty", "sixtyone", "sixtytwo", "sixtythree", "sixtyfour", "sixtyfive", "sixtysix", "sixtyseven", "sixtyeight", "sixtynine", "seventy", "seventyone", "seventytwo", "seventythree", "seventyfour", "seventyfive", "seventysix", "seventyseven", "seventyeight", "seventynine", "eighty", "eightyone", "eightytwo", "eightythree", "eightyfour", "eightyfive", "eightysix", "eightyseven", "eightyeight", "eightynine", "ninety", "ninetyone", "ninetytwo", "ninetythree", "ninetyfour", "ninetyfive", "ninetysix", "ninetyseven", "ninetyeight", "ninetynine")

  val hundreds = digits.map(_ + "hundred")
  val thousand = "onethousand"

  val result = (belowHundred ++ hundreds ++ hundreds.flatMap(h => belowHundred.map(h + "and" + _)) ++ thousand).mkString

  println(result.length)

}
