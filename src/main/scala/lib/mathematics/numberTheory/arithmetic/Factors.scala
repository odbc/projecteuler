package lib.mathematics.numberTheory.arithmetic

import lib.mathematics.numberTheory.numbers.{Naturals, Primes}

case class Factors(n: BigInt) {
  def primes: List[BigInt] = {
    val sqrt = Naturals.sqrt(n)
    Primes.sequence.takeWhile(_ <= sqrt).find(n % _ == 0).map(p => p :: Factors(n / p).primes).getOrElse(List(n))
  }

  def all: List[BigInt] = {
    val ps = primes
    1L :: List.range(1, ps.size + 1).flatMap(ps.combinations(_).map(_.product)).distinct
  }

  def canonical: Map[BigInt, BigInt] = primes.groupBy(identity).mapValues(_.size)
}
