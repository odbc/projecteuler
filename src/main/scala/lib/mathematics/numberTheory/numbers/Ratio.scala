package lib.mathematics.numberTheory.numbers

import Naturals.gcd

case class Ratio(num: BigInt, den: BigInt) {
  def /(other: Ratio) = Ratio(this.num * other.den, this.den * other.num)
  def ===(other: Ratio): Boolean = this.num * other.den == this.den * other.num

  def floor: Ratio =
    if (num >= 0) Ratio(num / den, 1)
    else Ratio(num / den - 1, 1)

  def contFraction: Stream[BigInt] = {
    def coefficients(a: BigInt, x: Ratio): Stream[(BigInt, Ratio)] = {
      if (x.num == 0) (a, x) #:: Stream.empty[(BigInt, Ratio)]
      else {
        val revX  = Ratio(x.den, x.num)
        val nextA = revX.floor
        (a, x) #:: coefficients(nextA.num, revX - nextA)
      }
    }

    coefficients(this.floor.num, this - this.floor).map(_._1)
  }

  override def toString: String =
    if (this.den == 1) this.num.toString
    else this.num.toString + "/" + this.den.toString
}

object Ratio {
  def apply(num: BigInt, den: BigInt): Ratio = {
    val g = gcd(num, den)
    val n = num / g
    val d = den / g
    if (d >= 0) new Ratio(n, d)
    else new Ratio(-n, -d)
  }

  implicit def ratioNumeric: Numeric[Ratio] = new Numeric[Ratio] {
    override def plus(x: Ratio, y: Ratio): Ratio = Ratio(x.num * y.den + x.den * y.num, x.den * y.den)
    override def minus(x: Ratio, y: Ratio): Ratio = Ratio(x.num * y.den - x.den * y.num, x.den * y.den)
    override def times(x: Ratio, y: Ratio): Ratio = Ratio(x.num * y.num, x.den * y.den)
    override def negate(x: Ratio): Ratio = times(x, Ratio(-1, 1))
    override def fromInt(x: Int): Ratio = Ratio(x, 1)
    override def toInt(x: Ratio): Int = (x.num / x.den).toInt
    override def toLong(x: Ratio): Long = (x.num / x.den).toLong
    override def toFloat(x: Ratio): Float = x.num.toFloat / x.den.toFloat
    override def toDouble(x: Ratio): Double = x.num.toDouble / x.den.toDouble
    override def compare(x: Ratio, y: Ratio): Int =
      if (x.num * y.den == x.den * y.num) 0
      else if (x.num * y.den > x.den * y.num) 1
      else -1
  }

  implicit def ratioNumericOps(lhs: Ratio): Numeric[Ratio]#Ops = ratioNumeric.mkNumericOps(lhs)
  implicit def ratioOrderingOps(lhs: Ratio): Ordering[Ratio]#Ops = ratioNumeric.mkOrderingOps(lhs)
}