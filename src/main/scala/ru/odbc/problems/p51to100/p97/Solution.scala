package ru.odbc.problems.p51to100.p97

object Solution extends App {

  def powMod(n: Long, exp: Long, mod: Long): Long = {
    def go(n: Long, exp: Long, mod: Long, acc: Long): Long =
      if (exp == 0) acc
      else go(n, exp - 1, mod, acc * n % mod)

    go(n, exp, mod, 1)
  }

  val lastCount = 10
  val power = 7830457
  val mod = BigInt(10).pow(lastCount).toLong

  val result = (28433 * powMod(2, power, mod) + 1)  % mod

  println(result)
}
