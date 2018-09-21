package owe.map.ops

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.{Actor, ActorRef}
import owe.entities.active.Walker.TraversalMode
import owe.entities.active.attributes.Distance
import owe.map.Cell.{Availability, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.pathfinding.Search

trait PathfindingOps { _: AvailabilityOps =>

  protected val search: Search
  protected implicit val ec: ExecutionContext

  //TODO - check if path should follow roads
  def generateAdvancePath(
    grid: Grid[CellActorRef],
    start: Point,
    end: Point,
    traversalMode: TraversalMode
  )(implicit sender: ActorRef = Actor.noSender): Future[Queue[Point]] =
    search
      .calculate(start, end, passableNeighboursOf(grid, _, traversalMode, roadblockRestricted = false))
      .map(_.drop(1))

  //TODO - check if path should follow roads
  def generateRoamPath(
    grid: Grid[CellActorRef],
    start: Point,
    maxDistance: Distance,
    traversalMode: TraversalMode
  )(implicit sender: ActorRef = Actor.noSender): Future[Queue[Point]] = {
    def extendPath(
      currentCell: Point,
      currentPath: Seq[Point],
      examined: Seq[Point],
      backtracked: Seq[Point]
    ): Future[Seq[Point]] = {
      val maxDistanceNotReached = currentPath.lengthCompare(maxDistance.value) < 0
      if (maxDistanceNotReached) {
        passableNeighboursOf(grid, currentCell, traversalMode, roadblockRestricted = true).flatMap { neighbours =>
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

  def passableNeighboursOf(
    grid: Grid[CellActorRef],
    cell: Point,
    traversalMode: TraversalMode,
    roadblockRestricted: Boolean
  )(implicit sender: ActorRef = Actor.noSender): Future[Seq[Point]] =
    Future
      .sequence(
        cell
          .neighbours(withCornerNeighbours = diagonalMovementAllowed(traversalMode))
          .map { point =>
            cellAvailabilityForPoint(grid, point).flatMap { availability =>
              if (availability >= Availability.Passable) {
                if (roadblockRestricted) {
                  cellHasRoadblock(grid, point).map { hasRoadblock =>
                    if (hasRoadblock) {
                      None
                    } else {
                      Some(point)
                    }
                  }
                } else {
                  Future.successful(Some(point))
                }
              } else {
                Future.successful(None)
              }
            }
          }
      )
      .map(_.flatten)

  def diagonalMovementAllowed(traversalMode: TraversalMode): Boolean =
    traversalMode match {
      case TraversalMode.RoadRequired  => false
      case TraversalMode.RoadPreferred => true // TODO - should follow road (no diagonal movement), if available
      case TraversalMode.OnLand        => true
      case TraversalMode.OnWater       => true
    }
}
