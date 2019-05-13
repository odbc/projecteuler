package commons

package object operations {

  def sqrt(n: BigInt): BigInt = Math.floor(Math.sqrt(n.toDouble)).toLong

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  def factorial(n: BigInt): BigInt =
    if (n < 0) 0
    else if (n == 0) 1
    else (BigInt(1) to n).product

}
