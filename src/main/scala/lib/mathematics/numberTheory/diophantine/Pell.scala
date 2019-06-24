package lib.mathematics.numberTheory.diophantine

import lib.mathematics.numberTheory.numbers.{Naturals, QuadraticIrrational}

/**
  * Pell's equation
  * x^2 - d * y^2 = b where d is a given positive nonsquare integer
  */
case class Pell(d: BigInt, b: BigInt) {

  def solutions: Stream[(BigInt, BigInt)] =
    if (Naturals.isSquare(d)) Stream.empty
    else if (b == 1) simplePell
    else {
      val eps = Pair(simplePell.head)

      val leftY  =
        if (b >= 0) 0
        else Math.ceil(Math.sqrt(-b.toDouble) / Math.sqrt(d.toDouble)).toInt
      val rightY =
        if (b >= 0) Math.floor(Math.sqrt(b.toDouble) * (eps.toNumber - Math.pow(eps.toNumber, -1)) / (2 * Math.sqrt(d.toDouble))).toInt
        else Math.floor(Math.sqrt(-b.toDouble) * (eps.toNumber + Math.pow(eps.toNumber, -1)) / (2 * Math.sqrt(d.toDouble))).toInt

      val basis = for {
        y <- leftY until rightY
        c = d * y * y + b
        if Naturals.isSquare(c)
      } yield Pair(Naturals.sqrt(c), y)

      def solutions(basisPair: Pair): Stream[Pair] = basisPair #:: solutions(basisPair * eps)

      def rotateSolutions(streams: Stream[Pair]*): Stream[Pair] =
        Stream(streams.map(_.head): _*) #::: rotateSolutions(streams.map(_.tail): _*)

      rotateSolutions(basis.map(solutions): _*).map { case Pair(x, y) => (x, y) }
    }

  private case class Pair(x: BigInt, y: BigInt) {
    def *(other: Pair) = Pair(this.x * other.x + d * this.y * other.y, this.x * other.y + this.y * other.x)

    def toNumber: Double = x.toDouble + y.toDouble * Math.sqrt(d.toDouble)
  }

  private case object Pair {
    def apply(a: (BigInt, BigInt)): Pair = new Pair(a._1, a._2)
  }

  private def simplePell: Stream[(BigInt, BigInt)] = {
    val fraction = QuadraticIrrational(0, 1, d, 1).contFraction

    lazy val ps: Stream[BigInt] = BigInt(1) #:: fraction.head #:: ps.zip(ps.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }
    lazy val qs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: qs.zip(qs.tail.zip(fraction.tail)).map { case (l, (r, ee)) => l + ee * r }

    val minResult = Pair(ps.tail.zip(qs.tail).dropWhile { case (p, q) => p * p - d * q * q != 1 }.head)

    def solutions(basisPair: Pair): Stream[Pair] = basisPair #:: solutions(basisPair * minResult)

    solutions(minResult).map(p => (p.x, p.y))
  }
}
