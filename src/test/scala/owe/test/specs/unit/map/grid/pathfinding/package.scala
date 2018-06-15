package owe.test.specs.unit.map.grid

import owe.map.grid.Point

package object pathfinding {
  private def pointInBounds(point: Point, x: Int, y: Int): Boolean =
    0 <= point.x && point.x <= x && 0 <= point.y && point.y <= y

  def neighboursOf(point: Point): Seq[Point] =
    owe.map.grid.pathfinding
      .neighboursOf(point, withCornerNeighbours = true)
      .collect {
        case Some(p) if pointInBounds(p, 2, 2) => p
      }
}
