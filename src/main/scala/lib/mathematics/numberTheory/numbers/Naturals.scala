package lib.mathematics.numberTheory.numbers

import scala.annotation.tailrec

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

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  def factorial(n: BigInt): BigInt =
    if (n < 0) 0
    else if (n == 0) 1
    else (BigInt(1) to n).product

  def digitsCount(n: Long): Int =
    if (n < 10L) 1
    else if (n < 100L) 2
    else if (n < 1000L) 3
    else if (n < 10000L) 4
    else if (n < 100000L) 5
    else if (n < 1000000L) 6
    else if (n < 10000000L) 7
    else if (n < 100000000L) 8
    else if (n < 1000000000L) 9
    else if (n < 10000000000L) 10
    else if (n < 100000000000L) 11
    else if (n < 1000000000000L) 12
    else if (n < 10000000000000L) 13
    else if (n < 100000000000000L) 14
    else if (n < 1000000000000000L) 15
    else if (n < 10000000000000000L) 16
    else if (n < 100000000000000000L) 17
    else 19
}
