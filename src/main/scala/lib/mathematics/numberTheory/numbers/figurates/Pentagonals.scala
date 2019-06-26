package lib.mathematics.numberTheory.numbers.figurates

import lib.mathematics.numberTheory.numbers.Naturals

case class Pentagonals(n: BigInt) {

  def isPentagonal: Boolean =
    if (n < 1) false
    else Naturals.isSquare(24 * n + 1) && (1 + Naturals.sqrt(24 * n + 1)) % 6 == 0
}

object Pentagonals {
  val sequence: Stream[BigInt] = Naturals().sequence.map(n => n * (3 * n - 1) / 2)
}