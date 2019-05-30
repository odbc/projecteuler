package commons

import numbers.naturalsFrom

import scala.annotation.tailrec

package object primes {

  val primes: Stream[BigInt] = 2L #:: naturalsFrom(3).filter(isPrime)

  def primeFactors(n: BigInt): List[BigInt] =
    primes.takeWhile(j => j * j <= n).find(n % _ == 0).map(p => p :: primeFactors(n / p)).getOrElse(List(n))

  def factors(n: BigInt): List[BigInt] = {
    val pf = primeFactors(n)

    1L :: List.range(1, pf.size + 1).flatMap(pf.combinations(_).map(_.product)).distinct
  }

  def isPrime(n: BigInt): Boolean =
    if (n < 2) false
    else primes.takeWhile(j => j * j <= n).forall(n % _ > 0)

  def canonicalRepresentation(n: BigInt): Map[BigInt, BigInt] = {
    @tailrec
    def go(num: BigInt, acc: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
      if (num == 1) acc
      else {
        val prime = primes.find(num % _ == 0).get
        go(num / prime, acc.updated(prime, BigInt(1) + acc.getOrElse(prime, 0)))
      }
    }

    go(n, Map.empty)
  }
}
