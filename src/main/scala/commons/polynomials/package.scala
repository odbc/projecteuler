package commons

package object polynomials {

  case class Polynomial[N](coefficients: Vector[N])(implicit num: Numeric[N]) {
    def unary_- = Polynomial(this.coefficients.map(num.negate))

    def +(other: Polynomial[N]): Polynomial[N] = Polynomial {
      this.coefficients.zipAll(other.coefficients, num.zero, num.zero).map { case (l, r) => num.plus(l, r) }
    }

    def -(other: Polynomial[N]): Polynomial[N] = this + -other

    def *(n: N): Polynomial[N] = Polynomial(this.coefficients.map(num.times(n, _)))

    def *(other: Polynomial[N]): Polynomial[N] =
      this.coefficients.foldLeft(Polynomial(Vector(num.zero))) { case (acc, c) => acc + other * c }

    def apply(x: N): N = {
      def pow(base: N, exp: Int): N =
        if (exp == 0) num.one
        else num.times(pow(base, exp - 1), base)

      this.coefficients.zipWithIndex.foldLeft(num.zero) { case (acc, (c, i)) =>
        num.plus(acc, num.times(c, pow(x, i)))
      }
    }
  }

  object Polynomial {
    def toPoly[N : Numeric](a: N): Polynomial[N] = Polynomial(Vector(a))
  }
}
