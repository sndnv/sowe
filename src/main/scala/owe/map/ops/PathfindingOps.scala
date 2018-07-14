package owe.map.ops

import owe.entities.active.attributes.Distance
import owe.map.Cell.{Availability, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.pathfinding.Search

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

trait PathfindingOps { _: AvailabilityOps =>

  protected val search: Search
  protected implicit val ec: ExecutionContext

  def passableNeighboursOf(grid: Grid[CellActorRef], cell: Point): Future[Seq[Point]] =
    //TODO - allow corner neighbours only for specific walkers that don't need roads
    //TODO - handle roadblocks for specific walkers
    Future
      .sequence(
        cell
          .neighbours(withCornerNeighbours = true)
          .map { point =>
            cellAvailability(grid, point).map { availability =>
              if (availability >= Availability.Passable) {
                Some(point)
              } else {
                None
              }
            }
          }
      )
      .map(_.flatten)

  //TODO - check if path should follow roads
  def generateAdvancePath(grid: Grid[CellActorRef], start: Point, end: Point): Future[Queue[Point]] =
    search.calculate(start, end, passableNeighboursOf(grid, _))

  //TODO - check if path should follow roads
  def generateRoamPath(grid: Grid[CellActorRef], start: Point, maxDistance: Distance): Future[Queue[Point]] = {
    def extendPath(
      currentCell: Point,
      currentPath: Seq[Point],
      examined: Seq[Point],
      backtracked: Seq[Point]
    ): Future[Seq[Point]] = {
      val maxDistanceNotReached = currentPath.lengthCompare(maxDistance.value) < 0
      if (maxDistanceNotReached) {
        passableNeighboursOf(grid, currentCell).flatMap { neighbours =>
          neighbours.filterNot { neighbour =>
            examined.contains(neighbour)
          }.headOption match {
            case Some(nextCell) =>
              extendPath(nextCell, currentPath :+ currentCell, examined :+ currentCell, backtracked)

            case None =>
              val backtrackedTooFar = backtracked.lengthCompare(currentPath.size / 2) >= 0
              if (backtrackedTooFar) {
                Future.successful((currentPath :+ currentCell) ++ backtracked.reverse)
              } else {
                currentPath.lastOption match {
                  case Some(previousCell) =>
                    extendPath(
                      previousCell,
                      currentPath.dropRight(1),
                      examined :+ currentCell,
                      backtracked :+ currentCell
                    )

                  case None =>
                    Future.successful(Seq.empty)
                }
              }
          }
        }
      } else {
        Future.successful(currentPath)
      }
    }

    extendPath(start, currentPath = Seq.empty, examined = Seq.empty, backtracked = Seq.empty).map(_.to[Queue])
  }
}
