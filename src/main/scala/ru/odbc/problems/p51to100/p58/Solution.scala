package ru.odbc.problems.p51to100.p58

import lib.mathematics.numberTheory.numbers.Primes

object Solution extends App {

  def squares(from: BigInt): Stream[BigInt] = (from * from) #:: squares(from + 1)

  def zipStreams[A](s1: Stream[A], s2: Stream[A], s3: Stream[A], s4: Stream[A]): Stream[List[A]] =
    (s1, s2, s3, s4) match {
      case (x #:: xs, y #:: ys, z #:: zs, a #:: as) => List(x, y, z, a) #:: zipStreams(xs, ys, zs, as)
      case _ => Stream.empty
    }

  val dr = squares(2).filter(_ % 2 == 1)
  val dl = dr.zipWithIndex.map { case (n, i) => n - 2 * i - 2 }
  val ur = dl.zipWithIndex.map { case (n, i) => n - 2 * i - 2 }
  val ul = ur.zipWithIndex.map { case (n, i) => n - 2 * i - 2 }

  val (_, _, r) = zipStreams(ul, ur, dl, dr).zipWithIndex.take(20000).foldLeft((0, 1, Int.MinValue)) {
    case ((ps, xs, ti), (l, i)) =>
      val primes = ps + l.count(n => Primes(n.toLong).isPrime)
      (primes, xs + 4, if (ti == Int.MinValue && primes.toDouble / (xs + 4) < 0.1) i else ti)
  }

  val result = 2 * r + 3

  println(result)
}
