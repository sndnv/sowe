package owe.map.ops

import owe.entities.active.Distance
import owe.map.MapCell
import owe.map.MapCell.Availability
import owe.map.grid.pathfinding.{neighboursOf, Search}
import owe.map.grid.{Grid, Point}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait PathingOps { _: AvailabilityOps =>

  protected val search: Search

  def passableNeighboursOf(grid: Grid[MapCell], cell: Point): Seq[Point] =
    //TODO - allow corner neighbours only for specific walkers that don't need roads
    //TODO - handle roadblocks for specific walkers
    neighboursOf(cell, withCornerNeighbours = true)
      .collect {
        case Some(point) if cellAvailability(grid, point) == Availability.Passable => point
      }

  //TODO - check if path should follow roads
  def generateAdvancePath(grid: Grid[MapCell], start: Point, end: Point): Option[Queue[Point]] =
    search.calculate(start, end, passableNeighboursOf(grid, _))

  //TODO - check if path should follow roads
  def generateRoamPath(grid: Grid[MapCell], start: Point, maxDistance: Distance): Option[Queue[Point]] = {
    @tailrec
    def extendPath(
      currentCell: Point,
      currentPath: Seq[Point],
      examined: Seq[Point],
      backtracked: Seq[Point]
    ): Seq[Point] = {
      val maxDistanceNotReached = currentPath.lengthCompare(maxDistance.value) < 0
      if (maxDistanceNotReached) {
        passableNeighboursOf(grid, currentCell).filterNot { neighbour =>
          examined.contains(neighbour)
        }.headOption match {
          case Some(nextCell) =>
            extendPath(nextCell, currentPath :+ currentCell, examined :+ currentCell, backtracked)

          case None =>
            val backtrackedTooFar = backtracked.lengthCompare(maxDistance.value / 2) >= 0
            if (backtrackedTooFar) {
              currentPath ++ backtracked
            } else {
              currentPath.lastOption match {
                case Some(previousCell) =>
                  extendPath(previousCell,
                             currentPath.dropRight(1),
                             examined :+ currentCell,
                             backtracked :+ currentCell)

                case None =>
                  Seq.empty
              }
            }
        }
      } else {
        currentPath
      }
    }

    extendPath(start, currentPath = Seq.empty, examined = Seq.empty, backtracked = Seq.empty) match {
      case Nil  => None
      case path => Some(path.to[Queue])
    }
  }
}
