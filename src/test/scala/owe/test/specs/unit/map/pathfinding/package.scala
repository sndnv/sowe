package owe.test.specs.unit.map

import owe.map.grid.Point

import scala.concurrent.Future

package object pathfinding {
  private def pointInBounds(point: Point, x: Int, y: Int): Boolean =
    0 <= point.x && point.x <= x && 0 <= point.y && point.y <= y

  def neighboursOf(point: Point): Future[Seq[Point]] =
    Future.successful(
      point
        .neighbours(withCornerNeighbours = true)
        .collect {
          case p if pointInBounds(p, 2, 2) => p
        }
    )
}
