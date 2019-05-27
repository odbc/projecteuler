package commons

import commons.numbers.isSquare
import commons.operations.gcd

package object equations {

  /**
    * Pell's equation
    * x^2 - d * y^2 = 1 where d is a given positive nonsquare integer
    */
  def pell(d: BigInt): Stream[(BigInt, BigInt)] =
    if (isSquare(d)) Stream.empty
    else {
      final case class Coeffs(a: BigInt, k1: BigInt, k2: BigInt, k3: BigInt)

      def contFraction(x: BigInt, coeffs: Coeffs): Stream[Coeffs] = {
        val a = Math.floor((coeffs.k1.toDouble * Math.sqrt(x.toDouble) + coeffs.k2.toDouble) / coeffs.k3.toDouble).toLong
        val k1 = coeffs.k1 * coeffs.k3
        val k2 = coeffs.k3 * coeffs.k3 * a - coeffs.k2 * coeffs.k3
        val k3 = coeffs.k1 * coeffs.k1 * x - coeffs.k2 * coeffs.k2 + 2 * coeffs.k2 * coeffs.k3 * a - coeffs.k3 * coeffs.k3 * a * a
        val g = gcd(gcd(k1, k2), k3).toInt
        coeffs #:: contFraction(x, Coeffs(a, k1 / g, k2 / g, k3 / g))
      }

      case class Pair(x: BigInt, y: BigInt) {
        def *(other: Pair) = Pair(this.x * other.x + d * this.y * other.y, this.x * other.y + this.y * other.x)

        def toNumber: Double = x.toDouble + y.toDouble * Math.sqrt(d.toDouble)
      }

      object Pair {
        def apply(p: (BigInt, BigInt)): Pair = Pair(p._1, p._2)
      }

      val a0 = BigInt(Math.floor(Math.sqrt(d.toLong)).toLong)
      val fraction = contFraction(d, Coeffs(a0, 1, a0, d - a0 * a0)).map(_.a)

      lazy val ps: Stream[BigInt] = BigInt(1) #:: fraction.head #:: ps.zip(ps.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }
      lazy val qs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: qs.zip(qs.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }

      val minResult = Pair(ps.tail.zip(qs.tail).dropWhile { case (p, q) => p * p - d * q * q != 1 }.head)

      def solutions(basisPair: Pair): Stream[Pair] = basisPair #:: solutions(basisPair * minResult)

      solutions(minResult).map(p => (p.x, p.y))
    }

  /**
    * Generalized Pell's equation
    * x^2 - d * y^2 = b where d is a given positive nonsquare integer
    */
  def generalizedPell(d: BigInt, b: BigInt): Stream[(BigInt, BigInt)] =
    if (isSquare(d)) Stream.empty
    else {
      case class Pair(x: BigInt, y: BigInt) {
        def *(other: Pair) = Pair(this.x * other.x + d * this.y * other.y, this.x * other.y + this.y * other.x)

        def toNumber: Double = x.toDouble + y.toDouble * Math.sqrt(d.toDouble)
      }

      val eps = Pair(2, 1)

      val leftY  = 0
      val rightY = Math.floor(Math.sqrt(b.toDouble) * (eps.toNumber - Math.pow(eps.toNumber, -1)) / (2 * Math.sqrt(d.toDouble))).toInt

      val basis = for {
        y <- leftY to rightY
        c = d * y + b
        if isSquare(c)
        x = Math.sqrt(c.toDouble).toInt
      } yield Pair(x, y)

      def solutions(basisPair: Pair): Stream[Pair] = basisPair #:: solutions(basisPair * eps)

      def rotateSolutions(streams: Stream[Pair]*): Stream[Pair] =
        Stream(streams.map(_.head): _*) #::: rotateSolutions(streams.map(_.tail): _*)

      rotateSolutions(basis.map(solutions): _*).map { case Pair(x, y) => (x, y) }
    }
}
