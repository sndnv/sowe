package owe.map.ops

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.entities.active.Walker.TraversalMode
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.{Cell, MapEntity}

import scala.concurrent.{ExecutionContext, Future}

trait AvailabilityOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def cellAvailability(
    cell: CellActorRef
  )(implicit sender: ActorRef): Future[Availability] =
    (cell ? GetCellAvailability()).mapTo[Availability]

  def cellAvailability(
    grid: Grid[CellActorRef],
    cell: Point
  )(implicit sender: ActorRef): Option[Future[Availability]] =
    grid
      .get(cell)
      .map(cellAvailability)

  def findFirstAdjacentPoint(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    matchesAvailability: Availability => Boolean
  )(implicit sender: ActorRef): Future[Option[Point]] =
    (for {
      parentPoint <- entities.get(entityID)
      parentCell <- grid.get(parentPoint)
    } yield {
      (parentCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(mapEntity) =>
          val cells = Entity.cells(mapEntity.spec.`size`, parentPoint)
          Future
            .sequence(
              cells
                .flatMap(point => grid.indexes().window(point, radius = 1).toSeq)
                .distinct
                .flatMap(point => grid.get(point).map(cell => (point, cell)))
                .collect {
                  case (point, cell) if !cells.contains(point) =>
                    cellAvailability(cell).map { availability =>
                      if (matchesAvailability(availability)) {
                        Some(point)
                      } else {
                        None
                      }
                    }
                }
            )
            .map(_.flatten.sorted.headOption)

        case None => Future.successful(None)
      }
    }).getOrElse(Future.successful(None))

  def isTraversable(
    traversalMode: TraversalMode,
    roadblockRestricted: Boolean,
    availability: Availability
  ): Boolean =
    traversalMode match {
      case TraversalMode.RoadRequired =>
        val hasRoad =
          availability.entityTypes.contains(Entity.Type.Road)

        val affectedByRoadblock =
          roadblockRestricted && availability.entityTypes.contains(Entity.Type.Roadblock)

        hasRoad && !affectedByRoadblock

      case TraversalMode.OnLand | TraversalMode.RoadPreferred =>
        availability.cellType == Cell.Type.Land || availability.cellType == Cell.Type.Floodplain

      case TraversalMode.OnWater =>
        availability.cellType == Cell.Type.Water
    }
}
