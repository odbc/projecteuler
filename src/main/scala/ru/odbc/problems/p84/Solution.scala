package ru.odbc.problems.p84

object Solution extends App {

  val dl = 6      // diceLimit

  val dices = for {
    first  <- 1 to dl
    second <- 1 to dl
  } yield (first, second)

  val G2J = 30
  val CCs = Set(2, 17, 33)
  val CHs = Set(7, 22, 36)

  val equation = Array.fill(40, 40)(0.0)

  for {
    target <- 0 to 39
    source <- 0 to 39
  } {
    if (source == target) equation(target)(source) = -1
    else {
      val sourceProps = dices.map { case (dice1, dice2) =>
        val doubles3Prop    = if (dice1 == dice2) 1.0 / (dl * dl * dl * dl) else 0.0
        val notDoubles3Prop = 1 - doubles3Prop

        val turn    = dice1 + dice2
        val endTurn = (source + turn) % 40

        val endsProp =
          if (endTurn != 30 && !CCs.contains(endTurn) && !CHs.contains(endTurn)) Map(endTurn -> notDoubles3Prop)
          else {
            if (endTurn == 30) Map(10 -> notDoubles3Prop)
            else if (CCs.contains(endTurn)) Map(
              0  -> notDoubles3Prop * 1.0 / 16,
              10 -> notDoubles3Prop * 1.0 / 16,
              endTurn -> notDoubles3Prop * 14.0 / 16,
            )
            else if (endTurn == 7) Map(
              0  -> (notDoubles3Prop * 1.0 / 16),
              10 -> (notDoubles3Prop * 1.0 / 16),
              11 -> (notDoubles3Prop * 1.0 / 16),
              24 -> (notDoubles3Prop * 1.0 / 16),
              39 -> (notDoubles3Prop * 1.0 / 16),
              5  -> (notDoubles3Prop * 1.0 / 16),
              15 -> (notDoubles3Prop * 2.0 / 16),
              12 -> (notDoubles3Prop * 1.0 / 16),
              4  -> (notDoubles3Prop * 1.0 / 16),
              7  -> (notDoubles3Prop * 6.0 / 16),
            )
            else if (endTurn == 22) Map(
              0  -> (notDoubles3Prop * 1.0 / 16),
              10 -> (notDoubles3Prop * 1.0 / 16),
              11 -> (notDoubles3Prop * 1.0 / 16),
              24 -> (notDoubles3Prop * 1.0 / 16),
              39 -> (notDoubles3Prop * 1.0 / 16),
              5  -> (notDoubles3Prop * 1.0 / 16),
              25 -> (notDoubles3Prop * 2.0 / 16),
              28 -> (notDoubles3Prop * 1.0 / 16),
              19 -> (notDoubles3Prop * 1.0 / 16),
              22 -> (notDoubles3Prop * 6.0 / 16),
            )
            else Map(
              0  -> (notDoubles3Prop * (1.0 / 16 + 1.0 / 256)),
              10 -> (notDoubles3Prop * (1.0 / 16 + 1.0 / 256)),
              11 -> (notDoubles3Prop * 1.0 / 16),
              24 -> (notDoubles3Prop * 1.0 / 16),
              39 -> (notDoubles3Prop * 1.0 / 16),
              5  -> (notDoubles3Prop * 3.0 / 16),
              12 -> (notDoubles3Prop * 1.0 / 16),
              33 -> (notDoubles3Prop * 14.0 / 256),
              36 -> (notDoubles3Prop * 6.0 / 16),
            )
          }

        val resProps = endsProp.map { case (t, v) => (t, if (t == 10) v + doubles3Prop else v) }
        resProps.getOrElse(target, 0.0)
      }
      equation(target)(source) = sourceProps.sum / (dl * dl)
    }
  }

  println(equation.head/*.map(e => f"${e + 0.025}%1.10f")*/.map(_ + 1).mkString(" ") + " 1")
  equation.tail.foreach(row => println(row/*.map(e => f"${e + 0.025}%1.10f")*/.mkString(" ") + " 0"))
  println(equation.map(_.toList).toList)

  /*

  4667087834067286137672422160172359738146165680740344255784674875368417220344764285262188153789739661606813354451588022297287781211735213382486964614020269092438761778437480848574257710822810838236/199460693357057436988106180956498285494084321480618662760499493951454521723556906249952546400453548837668373428562052542128392071134199863128772377642358010877310055080211888290321255585774372970001

  val linEqs = (0 to 39).map { n =>
    val equation = Array.fill(40)(0)
    equation(n) = -1
    for { i <- (n + 28) to (n + 33) } equation(i % 40) = i - n - 27
    for { i <- (n + 34) to (n + 38) } equation(i % 40) = -i + n + 39
    equation
  }

  println(linEqs.map(_.toList).toList)*/

  /*val text = (for {
    row <- linEqs
  } yield {
    val rowStr = row.zipWithIndex.map { case (coef, i) =>
      if (coef == 0) ""
      else if (coef == 1) "+p" + i.toString
      else if (coef == -1) "-p" + i.toString
      else if (coef > 0) "+" + coef + "p" + i.toString
      else "-" + coef + "p" + i.toString
    }.mkString + "=0"

    if (rowStr.startsWith("-")) rowStr else rowStr.tail
  }).mkString("\n") + "\n" +  (0 to 39).map(n => "p" + n.toString).mkString("+") + "=1"*/

  /*val text = (for {
    row <- linEqs
  } yield {
    row.map(e => if (e == -1) "-1" else if (e == 0) "0" else e.toString + "/36").mkString(" ") + " 0\n"
  }).mkString + Vector.fill(40)(1).mkString(" ") + " 1"

  println(text)*/

  /**
    * https://matrixcalc.org/en/slu.html
    */

  /*
  val squares = List(
    0  -> "GO",  1  -> "A1",  2  -> "CC1",  3  -> "A2",
    4  -> "T1",  5  -> "R1",  6  -> "B1",   7  -> "CH1",
    8  -> "B2",  9  -> "B3",  10 -> "JAIL", 11 -> "C1",
    12 -> "U1",  13 -> "C2",  14 -> "C3",   15 -> "R2",
    16 -> "D1",  17 -> "CC2", 18 -> "D2",   19 -> "D3",
    20 -> "FP",  21 -> "E1",  22 -> "CH2",  23 -> "E2",
    24 -> "E3",  25 -> "R3",  26 -> "F1",   27 -> "F2",
    28 -> "U2",  29 -> "F3",  30 -> "G2J",  31 -> "G1",
    32 -> "G2",  33 -> "CC3", 34 -> "G3",   35 -> "R4",
    36 -> "CH3", 37 -> "H1",  38 -> "T2",   39 -> "H2",
  )

  val dices = Map(2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4, 6 -> 5, 7 -> 6, 8 -> 5, 9 -> 4, 10 -> 3, 11 -> 2, 12 -> 1)
  */

}
