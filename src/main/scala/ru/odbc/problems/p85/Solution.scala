package ru.odbc.problems.p85

object Solution extends App {

  val cache = scala.collection.mutable.Map[(Int, Int), Int]()

  def rects(w: Int, h: Int): Int =
    if (w == 1 && h == 1) cache.getOrElseUpdate((1, 1), 1)
    else if (w > h) cache.getOrElseUpdate((w, h), rects(w - 1, h) + w * h * (h + 1) / 2)
    else cache.getOrElseUpdate((w, h), rects(w, h - 1) + w * (w + 1) * h / 2)

  val result = (for {
    w <- Stream.from(1)
    h <- 1 to w
  } yield (w, h, rects(w, h))).map { case (i, j, r) =>
    (i, j, Math.abs(2000000 - r))
  }.take(100000).minBy(_._3)

  println(result._1 * result._2)
}
