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

  val equation = Array.fill(40, 41)(0.0)

  val fromProbs = for {
    source <- 0 to 39
  } yield (source, {
    dices.map { case (dice1, dice2) =>
      val doubles3Prop    = if (dice1 == dice2) 1.0 / (dl * dl) else 0.0
      val notDoubles3Prop = 1 - doubles3Prop

      val turn    = dice1 + dice2
      val endTurn = (source + turn) % 40

      val endsProp =
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
        else if (endTurn == 36) Map(
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
        else
          Map(endTurn -> notDoubles3Prop)

      val targetProbs = endsProp.get(10) match {
        case Some(e) => endsProp.updated(10, e + doubles3Prop)
        case None    => endsProp + (10 -> doubles3Prop)
      }

      targetProbs filter { case (_, v) => v > 0 } mapValues { _ / (dl * dl) }
    }.foldLeft(Map[Int, Double]()) { case (acc, diceProbs) =>
      diceProbs.foldLeft(acc) { case (mapAcc, (t, p)) =>
        mapAcc.get(t) match {
          case Some(e) => mapAcc.updated(t, e + p)
          case None    => mapAcc + (t -> p)
        }
      }
    }
  })

  println(fromProbs)

  /*  for {
    target <- 0 to 39
    source <- 0 to 39
  } {
    val sourceProps = dices.map { case (dice1, dice2) =>
      val doubles3Prop    = if (dice1 == dice2) 1.0 / (dl * dl) else 0.0
      val notDoubles3Prop = 1 - doubles3Prop

      val turn    = dice1 + dice2
      val endTurn = (source + turn) % 40

      val endsProp =
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
        else if (endTurn == 36) Map(
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
        else
          Map(endTurn -> notDoubles3Prop)

      val resProps = endsProp.map { case (t, v) => (t, if (t == 10) v + doubles3Prop else v) }
      resProps.getOrElse(target, 0.0)
    }
    equation(target)(source) = sourceProps.sum / (dl * dl) - (if (source == target) 1.0 else 0.0)
  }

  equation(0) = equation(0).map(_ + 1)
  equation.foreach(row => println(row.map(f => f"$f%1.3f").mkString(" ")))*/

  /*println(equation.head.map(_ + 1).mkString(" ") + " 1")
  equation.tail.foreach(row => println(row.mkString(" ") + " 0"))
  println(equation.map(_.toList).toList)*/

  /**
    * https://matrixcalc.org/en/slu.html
    */

}
