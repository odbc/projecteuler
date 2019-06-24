package lib.mathematics.numberTheory.numbers

case class Naturals(from: BigInt = 1) {

  val sequence: Stream[BigInt] = naturalsFrom(from)

  private def naturalsFrom(n: BigInt): Stream[BigInt] = n #:: naturalsFrom(n + 1)
}

object Naturals {
  def sqrt(n: BigInt): BigInt = Math.sqrt(n.toDouble).toLong

  def isSquare(n: BigInt): Boolean = {
    val s = sqrt(n)
    s * s == n
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  def factorial(n: BigInt): BigInt =
    if (n < 0) 0
    else if (n == 0) 1
    else (BigInt(1) to n).product
}
