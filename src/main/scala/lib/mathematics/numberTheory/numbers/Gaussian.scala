package lib.mathematics.numberTheory.numbers

case class Gaussian(a: BigInt, b: BigInt) {
  def +(other: Gaussian) = Gaussian(this.a + other.a, this.b + other.b)
  def -(other: Gaussian) = Gaussian(this.a - other.a, this.b - other.b)
  def *(other: Gaussian) = Gaussian(this.a * other.a - this.b * other.b, this.a * other.b + this.b * other.a)

  def ==(other: Gaussian): Boolean = this.a == other.a && this.b == other.b

  def pow(exp: BigInt): Gaussian =
    if (exp < 0) Gaussian(0, 0)
    else (BigInt(1) to exp).foldLeft(Gaussian(1, 0)) { case (acc, _) => acc * this }

  override def toString: String =
    if (b == 0) a.toString
    else if (a == 0) s"${b}i"
    else s"$a + ${b}i".replace("+ -", "- ").replace(" 1i", " i")
}

object Gaussian {
  def apply(pair: (BigInt, BigInt)): Gaussian = Gaussian(pair._1, pair._2)
}