package owe.map.grid

import scala.language.implicitConversions

case class Point(x: Int, y: Int) {
  def neighbours(withCornerNeighbours: Boolean): Seq[Option[Point]] =
    Point.neighboursOf(this, withCornerNeighbours)
}

object Point {
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
  implicit def ordering[A <: Point]: Ordering[A] = Ordering.by(p => (p.y, p.x))

  def neighboursOf(point: Point, withCornerNeighbours: Boolean): Seq[Option[Point]] = {
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
      )
    } else {
      Seq.empty
    }
  }
}
