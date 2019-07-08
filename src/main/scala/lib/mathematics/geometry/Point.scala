package lib.mathematics.geometry

case class Point(x: Double, y: Double) {

  def distTo(other: Point): Double = Math.sqrt(Math.pow(this.x - other.x, 2) + Math.pow(this.y - other.y, 2))
}
