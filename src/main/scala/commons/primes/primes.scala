package commons

import scala.annotation.tailrec
import scala.collection.mutable

package object primes {

  private[primes] case class PriorityQ[V]() {
    private def order(kv: (BigInt, V)): BigInt = -kv._1
    private val PQ = mutable.PriorityQueue[(BigInt, V)]()(Ordering.by(order))

    def insert(k: BigInt, v: V): PriorityQ[V] = {
      PQ.enqueue((k, v))
      this
    }

    def minKey(): BigInt = PQ.head._1
    def minKeyValue(): (BigInt, V) = PQ.head
    def deleteMinAndInsert(k: BigInt, v: V): PriorityQ[V] = {
      PQ.dequeue()
      insert(k, v)
    }

    override def toString: String = PQ.toString
  }

  private[primes] def sieve(stream: Stream[BigInt]): Stream[BigInt] = stream match {
    case Stream.Empty => Stream.empty
    case x #:: xs     =>
      def insertPrime(p: BigInt, xs: Stream[BigInt], table: PriorityQ[Stream[BigInt]]): PriorityQ[Stream[BigInt]] =
        table.insert(p * p, xs.map(_ * p))

      def sievePQ(str: Stream[BigInt], table: PriorityQ[Stream[BigInt]]): Stream[BigInt] = str match {
        case Stream.Empty => Stream.empty
        case y #:: ys     =>
          val nextComposite = table.minKey()

          def adjust(table: PriorityQ[Stream[BigInt]]): PriorityQ[Stream[BigInt]] = {
            val (n, n1 #:: ns) = table.minKeyValue()
            if (n <= y) adjust(table.deleteMinAndInsert(n1, ns))
            else table
          }

          if (nextComposite <= y) sievePQ(ys, adjust(table))
          else y #:: sievePQ(ys, insertPrime(y, ys, table))
      }

      x #:: sievePQ(xs, insertPrime(x, xs, PriorityQ[Stream[BigInt]]()))
  }

  private[primes] def wheel2357(): Stream[BigInt] = BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(6) #:: BigInt(8) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(8) #:: BigInt(6) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(10) #:: BigInt(2) #:: BigInt(10) #:: wheel2357()
  private[primes] def spin(stream: Stream[BigInt], n: BigInt): Stream[BigInt] = n #:: spin(stream.tail, n + stream.head)
  val primes: Stream[BigInt] = BigInt(2) #:: BigInt(3) #:: BigInt(5) #:: BigInt(7) #:: sieve(spin(wheel2357(), 11))

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
