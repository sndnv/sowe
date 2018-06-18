package owe.map.grid

import scala.language.implicitConversions

case class Point(x: Int, y: Int) {
  def neighbours(withCornerNeighbours: Boolean): Seq[Point] =
    Point.neighboursOf(this, withCornerNeighbours)

  def distanceBetween(otherPoint: Point): Double = Point.distanceBetween(this, otherPoint)
}

object Point {
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def ordering[A <: Point]: Ordering[A] = Ordering.by(p => (p.y, p.x))

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

  def distanceBetween(point1: Point, point2: Point): Double = {
    val x = (point2.x - point1.x).abs
    val y = (point2.y - point1.y).abs

    Math.sqrt(x * x + y * y)
  }
}
