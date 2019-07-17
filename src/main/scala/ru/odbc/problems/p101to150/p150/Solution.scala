package ru.odbc.problems.p101to150.p150

object Solution extends App {

  val mod = BigInt(2).pow(20)
  val m = mod / 2

  val limit = 1000

  val (_, s) = (0 until limit * (limit + 1) / 2).foldLeft((BigInt(0), Vector.empty[BigInt])) { case ((t, v), _) =>
    val next = (BigInt(615949) * t + BigInt(797807)) % mod
    (next, v :+ next - m)
  }

  val (_, triangle) = (1 to limit).foldLeft((s, Vector.empty[Vector[BigInt]])) { case ((rest, acc), k) =>
    (rest.drop(k), acc :+ rest.take(k))
  }

  val result = (for {
    rowIndex <- 0 until limit
    elIndex  <- 0 to rowIndex
  } yield {
    (rowIndex + 1 until limit).scanLeft(triangle(rowIndex)(elIndex)) { case (acc, ri) =>
      acc + triangle(ri).slice(elIndex, elIndex + ri - rowIndex + 1).sum
    }.min
  }).min

  println(result)
}
