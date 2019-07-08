package ru.odbc.problems.p101to150.p144

import lib.mathematics.geometry.{Point, Vector}

object Solution extends App {

  val a = 5
  val b = 10

  def hits(init: Point, firstHit: Point): Stream[Point] = {
    val initVector = Vector(firstHit, init)
    val normal = Vector(b * b * firstHit.x, a * a * firstHit.y)
    val angle = 2 * initVector.angleTo(normal) * Math.signum(initVector.x * normal.y - initVector.y * normal.x)

    val A = initVector.x * Math.sin(angle) + initVector.y * Math.cos(angle)
    val B = initVector.y * Math.sin(angle) - initVector.x * Math.cos(angle)
    val C = -B * firstHit.y - A * firstHit.x

    val den   = a * a * A * A + b * b * B * B
    val discr = a * b * Math.sqrt(den - C * C)

    val root1 = Point((-a * a * A * C + discr * B) / den, (-b * b * B * C - discr * A) / den)
    val root2 = Point((-a * a * A * C - discr * B) / den, (-b * b * B * C + discr * A) / den)

    val next = List(root1, root2).maxBy(_.distTo(firstHit))
    firstHit #:: hits(firstHit, next)
  }

  val result = hits(Point(0.0, 10.1), Point(1.4, -9.6)).takeWhile { case Point(x, y) =>
    y < 0 || x >= 0.01 || x <= -0.01
  }.size

  println(result)
}
