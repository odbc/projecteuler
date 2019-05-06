package commons

package object numbers {

  def naturalsFrom(n: Long): Stream[Long] = n #:: naturalsFrom(n + 1)

  val naturals: Stream[Long] = naturalsFrom(1L)

  val fibonacci: Stream[BigInt] = BigInt(1) #:: BigInt(2) #:: fibonacci.zip(fibonacci.tail).map { case (l, r) => l + r }

  def collatz(n: Long): Stream[Long] = n #:: collatz(if (n % 2 == 0) n / 2 else 3 * n + 1)

  def isSquare(n: Long): Boolean = {
    val s = operations.sqrt(n)
    s * s == n
  }

}
