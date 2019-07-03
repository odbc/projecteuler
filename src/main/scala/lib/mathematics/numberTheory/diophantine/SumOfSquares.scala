package lib.mathematics.numberTheory.diophantine

import lib.mathematics.numberTheory.arithmetic.Factors
import lib.mathematics.numberTheory.numbers.{Gaussian, Naturals, Primes}

/**
  * x^2 + y^2 = c where c is a given positive integer
  */
case class SumOfSquares(c: BigInt) {

  def solutions: List[(BigInt, BigInt)] =
    if (Primes(c).isPrime) {
      if (c == 2) List((1, 1))
      else if (c % 4 == 1)
        (BigInt(1) to Naturals.sqrt(c))
          .find(m => Naturals.isSquare(c - m * m))
          .map(m => (m, Naturals.sqrt(c - m * m)))
          .toList
      else List()
    } else {
      val (q, s) = Factors(c).canonical.partition { case (p, _) => p == 2 || p % 4 == 1 }
      if (s.exists(_._2 % 2 != 0)) List()
      else {
        val sn = Naturals.sqrt(Factors(s).n)
        val (twos, ps) = q.partition(_._1 == 2)
        val gs = ps.map { case (p, exp) =>
          val squares = SumOfSquares(p).solutions.head
          val (gMinus, gPlus) = (Gaussian(squares._1, -squares._2), Gaussian(squares))
          for {
            r1 <- (BigInt(0) to exp).toList
            r2 =  exp - r1
          } yield gMinus.pow(r1) * gPlus.pow(r2)
        }.toList
        val gProds =
          if (gs.isEmpty) List()
          else gs.tail.foldLeft(gs.head) { case (acc, gl) => acc.flatMap(g => gl.map(_ * g)) }
        val resGs =
          if (twos.isEmpty) gProds
          else gProds.map(g => g * Gaussian(1, 1).pow(twos.head._2))

        resGs
          .map { case Gaussian(a, b) => Set((if (a >= 0) a else -a) * sn, (if (b >= 0) b else -b) * sn) }.distinct
          .map { s =>
            val l = s.toList
            (l.head, l.last)
          }
          .filter { case (a, b) => a != 0 && b != 0 }
      }
    }
}
