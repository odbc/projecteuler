package lib.mathematics.numberTheory.numbers.figurates

import lib.mathematics.numberTheory.numbers.Naturals

case class Hexagonals(n: BigInt)

object Hexagonals {
  val sequence: Stream[BigInt] = Naturals().sequence.map(n => n * (2 * n - 1))
}

