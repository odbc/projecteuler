package lib.mathematics.geometry

import lib.mathematics.numberTheory.diophantine.SumOfSquares
import lib.mathematics.numberTheory.numbers.Naturals

case class Pythagoreans(a: BigInt, b: BigInt, c: BigInt) {
  val perimeter: BigInt = a + b + c
}

object Pythagoreans {
  val sequence: Stream[Pythagoreans] = Naturals(2).sequence.flatMap { c =>
    SumOfSquares(c * c).solutions
      .filter { case (a, b) => a != 0 && b != 0 }
      .map { case (a, b) => Pythagoreans(a, b, c) }
      .toStream
  }
}