package commons

import numbers.naturalsFrom

package object primes {

  val primes: Stream[Long] = 2L #:: naturalsFrom(3L).filter(isPrime)

  def primeFactors(n: Long): List[Long] =
    primes.takeWhile(j => j * j <= n).find(n % _ == 0).map(p => p :: primeFactors(n / p)).getOrElse(List(n))

  def factors(n: Long): List[Long] = {
    val pf = primeFactors(n)

    1L :: List.range(1, pf.size + 1).flatMap(pf.combinations(_).map(_.product)).distinct
  }

  def isPrime(n: Long): Boolean = primes.takeWhile(j => j * j <= n).forall(n % _ > 0)

}
