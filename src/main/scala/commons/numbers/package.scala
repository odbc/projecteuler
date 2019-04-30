package commons

package object numbers {

  def naturalsFrom(n: Long): Stream[Long] = n #:: naturalsFrom(n + 1)

  val naturals: Stream[Long] = naturalsFrom(1L)

  val fibonacci: Stream[Long] = 1L #:: 2L #:: fibonacci.zip(fibonacci.tail).map { case (l, r) => l + r }

  def collatz(n: Long): Stream[Long] = n #:: collatz(if (n % 2 == 0) n / 2 else 3 * n + 1)

}
