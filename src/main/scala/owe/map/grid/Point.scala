package owe.map.grid

import scala.language.implicitConversions

case class Point(x: Int, y: Int) {
  def neighbours(withCornerNeighbours: Boolean): Seq[Point] = Point.neighboursOf(this, withCornerNeighbours)

  def isDirectNeighbourOf(other: Point): Boolean = Point.areDirectNeighbours(this, other)

  def distanceBetween(otherPoint: Point): Double = Point.distanceBetween(this, otherPoint)
}

object Point {
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def ordering[A <: Point]: Ordering[A] = Ordering.by(p => (p.y, p.x))

  def areDirectNeighbours(a: Point, b: Point): Boolean =
    distanceBetween(a, b) == 1.0

  def neighboursOf(point: Point, withCornerNeighbours: Boolean): Seq[Point] = {
    val Point(x, y) = point

    if (x >= 0 && y >= 0) {
      Seq(
        /* top    left   */ if (withCornerNeighbours && x > 0 && y > 0) Some((x - 1, y - 1)) else None,
        /* top    center */ if (y > 0) Some((x, y - 1)) else None,
        /* top    right  */ if (withCornerNeighbours && y > 0) Some((x + 1, y - 1)) else None,
        /* middle left   */ if (x > 0) Some((x - 1, y)) else None,
        /* middle right  */ Some((x + 1, y)),
        /* bottom left   */ if (withCornerNeighbours && x > 0) Some((x - 1, y + 1)) else None,
        /* bottom center */ Some((x, y + 1)),
        /* bottom right  */ if (withCornerNeighbours) Some((x + 1, y + 1)) else None
      ).flatMap(point => point.map(t => Point(t._1, t._2)))
    } else {
      Seq.empty
    }
  }

  def distanceBetween(a: Point, b: Point): Double = {
    val x = (b.x - a.x).abs
    val y = (b.y - a.y).abs

    Math.sqrt(x * x + y * y)
  }
}
