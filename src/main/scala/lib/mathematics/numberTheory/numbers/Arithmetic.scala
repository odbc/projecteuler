package lib.mathematics.numberTheory.numbers

case class Arithmetic(from: BigInt, difference: BigInt) {

  val sequence: Stream[BigInt] = progressionFrom(from)

  def sum(n: Long): BigInt = (2 * from + (n - 1) * difference) * n / 2

  private def progressionFrom(n: BigInt): Stream[BigInt] = n #:: progressionFrom(n + difference)
}
