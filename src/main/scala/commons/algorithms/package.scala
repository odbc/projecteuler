package commons

import scala.annotation.tailrec

package object algorithms {

  @tailrec
  def whileLoop[T](cond: T => Boolean)(f: T => T)(value: T): T =
    if (cond(value)) whileLoop(cond)(f)(f(value)) else value

}
