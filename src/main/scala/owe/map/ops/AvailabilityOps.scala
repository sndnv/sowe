package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.map.Cell._
import owe.map.MapEntity
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait AvailabilityOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def cellAvailability(cell: CellActorRef): Future[Availability] =
    (cell ? GetCellAvailability()).mapTo[Availability]

  def cellAvailability(grid: Grid[CellActorRef], cell: Point): Future[Availability] =
    grid
      .get(cell)
      .map(cellAvailability)
      .getOrElse(Future.successful(Availability.OutOfBounds))

  def cellHasRoad(cell: CellActorRef): Future[Boolean] =
    (cell ? HasRoad()).mapTo[Boolean]

  def requiredAvailability(entityType: Entity.Type): Availability = entityType match {
    case Entity.Type.Doodad    => Availability.Buildable
    case Entity.Type.Road      => Availability.Buildable
    case Entity.Type.Roadblock => Availability.Passable //TODO - can be built only on roads w/o walkers
    case Entity.Type.Resource  => Availability.Buildable
    case Entity.Type.Structure => Availability.Buildable
    case Entity.Type.Walker    => Availability.Passable
  }

  def findFirstAdjacentRoad(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef
  ): Future[Option[Point]] =
    (for {
      parentPoint <- entities.get(entityID)
      parentCell <- grid.get(parentPoint)
    } yield {
      (parentCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(mapEntity) =>
          val cells = entityCells(mapEntity.size, parentPoint)
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
    }) match {
      case Some(future) => future
      case None         => Future.successful(None)
    }

  def entityCells(entitySize: Entity.Size, parentCell: Point): Seq[Point] =
    (parentCell.x to entitySize.width)
      .flatMap(
        x => (parentCell.y to entitySize.height).map(y => Point(x, y))
      )
}
