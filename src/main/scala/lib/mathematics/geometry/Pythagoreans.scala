package lib.mathematics.geometry

import lib.mathematics.numberTheory.numbers.{Naturals, Primes, Gaussian}
import lib.mathematics.numberTheory.arithmetic.Factors

case class Pythagoreans(a: BigInt, b: BigInt, c: BigInt) {
  val perimeter:BigInt = a + b + c
}

object Pythagoreans {
  val sequence: Stream[Pythagoreans] = Naturals(2).sequence.flatMap { c =>
    compositeToSquares(c * c)
      .filter { case (a, b) => a != 0 && b != 0 }
      .map { case (a, b) => Pythagoreans(a, b, c) }
      .toStream
  }

  private def primeToSquares(p: BigInt): Option[(BigInt, BigInt)] = {
    if (!Primes(p).isPrime) None
    else if (p == 2) Some((1, 1))
    else if (p % 4 == 1)
      (BigInt(1) to Naturals.sqrt(p))
        .find(m => Naturals.isSquare(p - m * m))
        .map(m => (m, Naturals.sqrt(p - m * m)))
    else None
  }

  private def compositeToSquares(n: BigInt): List[(BigInt, BigInt)] = {
    val (q, s) = Factors(n).canonical.partition { case (p, _) => p == 2 || p % 4 == 1 }
    if (s.exists(_._2 % 2 != 0)) List()
    else {
      val sn = Naturals.sqrt(Factors(s).n)
      val (twos, ps) = q.partition(_._1 == 2)
      val gs = ps.map { case (p, exp) =>
        val squares = primeToSquares(p).get
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

      resGs.map { case Gaussian(a, b) => Set((if (a >= 0) a else -a) * sn, (if (b >= 0) b else -b) * sn) }.distinct
        .map { s =>
          val l = s.toList
          (l.head, l.last)
        }
    }
  }
}