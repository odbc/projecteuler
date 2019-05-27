package commons

import commons.operations.gcd

package object numbers {

  def naturalsFrom(n: BigInt): Stream[BigInt] = n #:: naturalsFrom(n + 1)

  val naturals: Stream[BigInt] = naturalsFrom(1)

  val fibonacci: Stream[BigInt] = BigInt(1) #:: BigInt(2) #:: fibonacci.zip(fibonacci.tail).map { case (l, r) => l + r }

  def collatz(n: Long): Stream[Long] = n #:: collatz(if (n % 2 == 0) n / 2 else 3 * n + 1)

  def isSquare(n: BigInt): Boolean = {
    val s = operations.sqrt(n)
    s * s == n
  }

  case class Ratio(num: BigInt, den: BigInt) {
    def +(other: Ratio) = Ratio(this.num * other.den + this.den * other.num, this.den * other.den)
    def -(other: Ratio) = Ratio(this.num * other.den - this.den * other.num, this.den * other.den)
    def *(other: Ratio) = Ratio(this.num * other.num, this.den * other.den)
    def /(other: Ratio) = Ratio(this.num * other.den, this.den * other.num)

    def ==(other: Ratio): Boolean = this.num * other.den == this.den * other.num
    def >(other: Ratio): Boolean  = this.num * other.den > this.den * other.num
    def <(other: Ratio): Boolean  = ! (this > other)

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
      override def plus(x: Ratio, y: Ratio): Ratio = x + y
      override def minus(x: Ratio, y: Ratio): Ratio = x - y
      override def times(x: Ratio, y: Ratio): Ratio = x * y
      override def negate(x: Ratio): Ratio = x * Ratio(-1, 1)
      override def fromInt(x: Int): Ratio = Ratio(x, 1)
      override def toInt(x: Ratio): Int = (x.num / x.den).toInt
      override def toLong(x: Ratio): Long = (x.num / x.den).toLong
      override def toFloat(x: Ratio): Float = x.num.toFloat / x.den.toFloat
      override def toDouble(x: Ratio): Double = x.num.toDouble / x.den.toDouble
      override def compare(x: Ratio, y: Ratio): Int =
        if (x == y) 0
        else if (x > y) 1
        else -1
    }
  }
}
