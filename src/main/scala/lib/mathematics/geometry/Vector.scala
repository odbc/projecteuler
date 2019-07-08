package lib.mathematics.geometry

case class Vector(x: Double, y: Double) {
  def norm: Double = Math.sqrt(this dot this)

  def dot(other: Vector): Double = this.x * other.x + this.y * other.y
  def angleTo(other: Vector): Double = Math.acos((this dot other) / (this.norm * other.norm))
}

object Vector {
  def apply(start: Point, end: Point): Vector = Vector(end.x - start.x, end.y - start.y)
}
