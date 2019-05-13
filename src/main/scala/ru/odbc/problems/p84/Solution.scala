package ru.odbc.problems.p84

import scala.collection.mutable

object Solution extends App {

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

  var probs = Array.fill(40)(BigInt(1))
  val sumProbs = probs.sum

  println(probs.toList)

  var newCur = mutable.Set[Int](0)

  (1 to 400).foreach { _ =>
    val cur = newCur.clone()
    newCur = mutable.Set[Int]()

    val g = probs.reduce(commons.operations.gcd)
    probs = probs.map(_ / g)

    for {
      c <- cur
      d <- dices.keys
      newProb = BigInt(dices(d)) * probs(c)
    } {
      newCur.add((c + d) % 40)
      probs((c + d) % 40) = newProb
    }
  }

  /*val newCur = mutable.Set[Int]()
  for {
    c <- cur
    d <- dices.keys
    newProb = probs(c) * dices(d) * 36000 / 36
  } {
    newCur.add(c + d)
    probs(c + d) = newProb
  }*/

  println(probs.toList)
  println(newCur)

}
