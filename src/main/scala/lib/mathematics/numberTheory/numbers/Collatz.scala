package lib.mathematics.numberTheory.numbers

case class Collatz(from: BigInt) {

  val sequence: Stream[BigInt] = collatzFrom(from)

  private def collatzFrom(n: BigInt): Stream[BigInt] = n #:: collatzFrom(if (n % 2 == 0) n / 2 else 3 * n + 1)
}