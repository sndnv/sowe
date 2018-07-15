package owe.map.ops

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.map.Cell._
import owe.map.MapEntity
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait AvailabilityOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def cellAvailability(
    cell: CellActorRef
  )(implicit sender: ActorRef = Actor.noSender): Future[Availability] =
    (cell ? GetCellAvailability()).mapTo[Availability]

  def cellAvailabilityForPoint(
    grid: Grid[CellActorRef],
    cell: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Availability] =
    grid
      .get(cell)
      .map(cellAvailability)
      .getOrElse(Future.successful(Availability.OutOfBounds))

  def cellHasRoad(
    cell: CellActorRef
  )(implicit sender: ActorRef = Actor.noSender): Future[Boolean] =
    (cell ? HasRoad()).mapTo[Boolean]

  def findFirstAdjacentRoad(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef
  )(implicit sender: ActorRef = Actor.noSender): Future[Option[Point]] =
    (for {
      parentPoint <- entities.get(entityID)
      parentCell <- grid.get(parentPoint)
    } yield {
      (parentCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(mapEntity) =>
          val cells = Entity.cells(mapEntity.size, parentPoint)
          Future
            .sequence(
              cells
                .flatMap(point => grid.indexes().window(point, radius = 1).toSeq)
                .distinct
                .flatMap(point => grid.get(point).map(cell => (point, cell)))
                .collect {
                  case (point, cell) if !cells.contains(point) =>
                    cellHasRoad(cell).map(if (_) Some(point) else None)
                }
            )
            .map(_.flatten.sorted.headOption)

        case None => Future.successful(None)
      }
    }).getOrElse(Future.successful(None))
}
