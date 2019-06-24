package lib.mathematics.numberTheory.numbers

import scala.collection.mutable

case class Primes(p: BigInt) {

  def isPrime: Boolean =
    if (p < 2) false
    else Primes.sequence.takeWhile(j => j * j <= p).forall(p % _ > 0)
}

object Primes {
  val sequence: Stream[BigInt] = BigInt(2) #:: BigInt(3) #:: BigInt(5) #:: BigInt(7) #:: sieve(spin(wheel2357(), 11))

  private case class PriorityQ[V]() {
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

  private def sieve(stream: Stream[BigInt]): Stream[BigInt] = stream match {
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

  private def wheel2357(): Stream[BigInt] = BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(6) #:: BigInt(8) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(8) #:: BigInt(6) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(6) #:: BigInt(2) #:: BigInt(6) #:: BigInt(4) #:: BigInt(2) #:: BigInt(4) #:: BigInt(2) #:: BigInt(10) #:: BigInt(2) #:: BigInt(10) #:: wheel2357()
  private def spin(stream: Stream[BigInt], n: BigInt): Stream[BigInt] = n #:: spin(stream.tail, n + stream.head)
}
