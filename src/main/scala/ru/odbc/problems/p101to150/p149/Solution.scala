package ru.odbc.problems.p101to150.p149

object Solution extends App {

  val limit = 2000

  def maxSum(list: Vector[BigInt]): BigInt =
    list.foldLeft((list.head, BigInt(0))) { case ((ans, sum), e) =>
      val nextSum = sum + e
      (List(ans, nextSum).max, List(nextSum, BigInt(0)).max)
    }._1

  val sInit = (BigInt(1) to BigInt(55)).map(k => (100003 - 200003 * k + 300007 * k * k * k) % 1000000 - 500000).toVector
  val s = (55 until 4000000).foldLeft(sInit) { case (v, k) =>
    v :+ (v(k - 24) + v(k - 55) + 1000000) % 1000000 - 500000
  }

  val hs = s.sliding(limit, limit).map(maxSum).max
  val vs = (0 until limit).map(n => maxSum((n until s.length by limit).toVector.map(s(_)))).max
  val ds = (0 until limit).toVector.map { n =>
    if (n == 0) maxSum((s.indices by limit + 1).toVector.map(s(_)))
    else Vector(
      maxSum((n until (limit - n) * limit by limit + 1).toVector.map(s(_))),
      maxSum((n * limit until s.length by limit + 1).toVector.map(s(_))),
    ).max
  }.max
  val ads = (0 until limit).toVector.map { n =>
    if (n == limit - 1) maxSum((n to n * limit by limit - 1).toVector.map(s(_)))
    else Vector(
      maxSum((n to n * limit by limit - 1).toVector.map(s(_))),
      maxSum((s.length - n * limit - 1 until s.length by limit - 1).toVector.map(s(_))),
    ).max
  }.max

  val result = List(hs, vs, ds, ads).max

  println(result)
}
