package lib.mathematics.numberTheory.numbers

case class Fibonacci(first: BigInt = 1, second: BigInt = 1) {

  val sequence: Stream[BigInt] = first #:: second #:: sequence.zip(sequence.tail).map { case (l, r) => l + r }
}
