package owe.test.specs.unit.map

import owe.map.grid.Point

package object pathfinding {
  private def pointInBounds(point: Point, x: Int, y: Int): Boolean =
    0 <= point.x && point.x <= x && 0 <= point.y && point.y <= y

  def neighboursOf(point: Point): Seq[Point] =
    point
      .neighbours(withCornerNeighbours = true)
      .collect {
        case Some(p) if pointInBounds(p, 2, 2) => p
      }
}
