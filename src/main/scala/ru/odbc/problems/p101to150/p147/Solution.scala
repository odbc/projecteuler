package ru.odbc.problems.p101to150.p147

object Solution extends App {

  val rows = 43
  val cols = 47

  val initWays = (1 to cols).map(c => ((1, c), BigInt(c * (c + 1) / 2 + c - 1))).toMap
  val ways = (1 to cols).foldLeft(initWays) { case (cws, c) =>
    (2 to List(rows, c).min).foldLeft(cws) { case (rws, r) =>
      val bDiag = (1 until c).map { p =>
        if (p >= r && c - p >= r) r * (2 * r - 1)
        else if (p < r && c - p >= r) p * (4 * r - 2 * p - 1)
        else if (p >= r && c - p < r) (c - p) * (4 * r - 2 * (c - p) - 1)
        else if (c <= r) 4 * p * (c - p)
        else 4 * p * (c - p) - (c - r) * (2 * c - 2 * r + 1)
      }.sum
      val uDiag = (1 to c).map { p =>
        if (p >= r && c - p + 1 >= r) (r - 1) * (2 * r - 1)
        else if (p < r && c - p + 1 >= r) (2 * p - 1) * (2 * r - p - 1)
        else if (p >= r && c - p + 1 < r) (2 * r - c + p - 2) * (2 * c - 2 * p + 1)
        else if (c <= r - 1) (2 * p - 1) * (2 * c - 2 * p + 1)
        else (2 * p - 1) * (2 * c - 2 * p + 1) - (c - r + 1) * (2 * c - 2 * r + 1)
      }.sum
      val nextWays = rws(r - 1, c) + rws(1, c) + (r - 1) * c * (c + 1) / 2 + bDiag + uDiag - c + 1
      rws + ((r, c) -> nextWays)
    }
  }

  val result = (for {
    r <- 1 to rows
    c <- 1 to cols
  } yield if (r < c) ways(r, c) else ways(c, r)).sum

  println(result)
}
