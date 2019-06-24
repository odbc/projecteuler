package lib

import scala.annotation.tailrec

package object aux {

  @tailrec
  def whileLoop[T](value: T)(cond: T => Boolean)(f: T => T): T =
    if (cond(value)) whileLoop(f(value))(cond)(f) else value

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + " ms")
    result
  }

}
