package lib.mathematics.numberTheory.numbers.figurates

import lib.mathematics.numberTheory.numbers.Naturals

case class Triangles(n: BigInt) {

  def isTriangle: Boolean =
    if (n < 1) false
    else Naturals.isSquare(8 * n + 1)
}

