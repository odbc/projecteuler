package commons

package object operations {

  def sqrt(n: Long): Long = Math.floor(Math.sqrt(n)).toLong

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

}
